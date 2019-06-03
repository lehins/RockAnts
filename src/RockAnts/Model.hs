{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module RockAnts.Model where

import Data.Foldable as F
import Data.Coerce
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Atomic
import RIO as P
import RIO.Set as Set
import RockAnts.Grid
import RockAnts.Random
import RockAnts.Types

walkAhead :: Ant -> Walk -> (Ix2 -> Maybe Nest -> RIO Env Walk) -> RIO Env Walk
walkAhead ant walk adjustWalk
  | walkStepsLeft walk > 0 = do
    adjustedDirection <- moveInDirection ant (walkDirection walk)
    pure Walk {walkStepsLeft = walkStepsLeft walk - 1, walkDirection = adjustedDirection}
  | otherwise = do
      currentLocation <- readIORef $ antLocation ant
      Grid {gridMap, gridNests} <- envGrid <$> ask
      cell <- gridMap !? currentLocation
      adjustWalk currentLocation (lookupNest gridNests cell)


walkInside :: Ant -> Nest -> Walk -> RIO Env Walk
walkInside ant nest walk =
  walkAhead ant walk $ \currentLocation mNest ->
    case mNest of
      Just someNest
        | nestIx someNest == nestIx nest -> walkInside ant nest =<< changeWalkDirection walk
        | otherwise -> goOutside ant someNest
      Nothing -> do
          destination <- nestEntranceDestination nest
          case destinationToWalk currentLocation destination of
            Nothing -> pure walk
            Just destinationWalk -> walkInside ant nest destinationWalk

walkOutside :: Ant -> Walk -> RIO Env Walk
walkOutside ant walk =
  walkAhead ant walk $ \_currentLocation mNest ->
    case mNest of
      Just someNest -> goOutside ant someNest
      Nothing -> walkOutside ant =<< changeWalkDirection walk

nestEntranceDestination :: HasGen env => Nest -> RIO env Ix2
nestEntranceDestination nest =
  fromMaybe (error "Impossible: empty nest entrance") <$> randomElement (nestEntranceRange nest)

goOutside :: Ant -> Nest -> RIO Env Walk
goOutside ant@Ant {..} nest = do
  currentLocation <- readIORef antLocation
  destination <- nestEntranceDestination nest
  case destinationToWalk currentLocation destination of
    Nothing -> pure $ Walk 0 0
    Just destinationWalk -> do
      adjustedDirection <- moveInDirection ant (walkDirection destinationWalk)
      pure destinationWalk {walkDirection = adjustedDirection}


changeWalkDirection :: Walk -> RIO Env Walk
changeWalkDirection Walk {..} = do
  newDirection <- randomDirection -- [0, 2*pi)
  Env {envConstants} <- ask
  numSteps <- randomIntRange (1, constMaxSteps envConstants) -- [1, maxSteps]
  pure Walk {walkDirection = newDirection, walkStepsLeft = numSteps}


clearCell :: Ix2 -> RIO Env ()
clearCell cellIx = do
  Colony {colonyGrid} <- envColony <$> ask
  unlessM (atomicWriteIntArray colonyGrid cellIx (coerce emptyCell)) $
    throwString $ "Impossible: attempt to clear a cell out of bounds: " <> show cellIx


moveInDirection :: Ant -> Double -> RIO Env Double
moveInDirection ant@Ant {..} direction = do
  logDebug $ display antIx <> " moving in direction: " <> display direction
  currentLocation <- readIORef antLocation
  -- Cell index where an attempt to move will happen.
  let targetLocation = cellInDirection currentLocation direction
  Colony {colonyGrid, colonyAnts} <- envColony <$> ask
  placeAntInCell colonyGrid targetLocation antIx >>= \case
    HasPlaced -> do
      logDebug $ display antIx <> " has moved into cell " <> displayShow targetLocation
      clearCell currentLocation
      writeIORef antLocation targetLocation
      addDirectionDelta direction
    HitWall -> randomTurn direction >>= moveInDirection ant
    HasOccupant occupantIx -> do
      occupant <- colonyAnts !? occupantIx
      hasSwapped <- tryToSwapWith ant currentLocation occupant targetLocation
      if hasSwapped
        then addDirectionDelta direction
        else randomTurn direction >>= moveInDirection ant

addDirectionDelta :: Double -> RIO Env Double
addDirectionDelta direction = do
  delta <- randomDoubleRangeInclusive (-pi/12, pi/12)
  pure (direction + delta)

randomTurn :: Double -> RIO Env Double
randomTurn direction = do
  turnLeft <- randomBool
  pure $
    if turnLeft
      then direction + (pi / 3 + pi / 216) -- Rock ants are slightly left biased
      else direction - pi / 3

withAntTask :: MonadIO m => Ant -> (Task -> m (Task, a)) -> m (Maybe a)
withAntTask Ant {antState} f = do
  state <- atomicModifyIORef' antState $ \case
    Idle task -> (Busy task, Idle task)
    state -> (state, state)
  case state of
    Idle task -> do
      (newTask, res) <- f task
      writeIORef antState (Idle newTask)
      pure $ Just res
    _ -> pure Nothing

withAntTask' :: MonadIO m => Ant -> (Task -> m a) -> m (Maybe a)
withAntTask' ant f = withAntTask ant f'
  where
    f' task = (task, ) <$> f task

withAntTask_ :: MonadIO m => Ant -> (Task -> m Task) -> m Bool
withAntTask_ ant f =
  isJust <$> withAntTask ant f'
  where
    f' task = (, ()) <$> f task

withAnt :: MonadIO m => Ant -> m a -> m (Maybe a)
withAnt ant f = withAntTask ant f'
  where
    f' task = (task, ) <$> f

withAntAt :: Ix2 -> (Ant -> Task -> RIO Env (Maybe (Task, a))) -> RIO Env (Maybe a)
withAntAt ix f = do
  Colony {colonyGrid, colonyAnts} <- envColony <$> ask
  mAntIx <- read' colonyGrid ix
  case colonyAnts !? mAntIx of
    Nothing -> pure Nothing
    Just ant@Ant {antLocation} -> do
      mRes <-
        withAntTask ant $ \task -> do
          ix' <- readIORef antLocation
          if ix == ix'
            then f ant task >>= \case
                   Nothing -> pure (task, Nothing)
                   Just (newTask, a) -> pure (newTask, Just a)
            else pure (task, Nothing)
      pure $ join mRes

forNeighbors :: Ant -> (Ant -> Task -> RIO Env (Maybe (Task, a))) -> RIO Env (Maybe a)
forNeighbors Ant{antLocation} f = do
  ix <- readIORef antLocation
  foldM action Nothing $ cellNeighbors ix
  where
    action mAnt@Just {} _ = pure mAnt
    action Nothing cellIx = withAntAt cellIx f

replaceAntInCell ::
     (MonadIO m, PrimMonad m) => MArray (PrimState m) P Ix2 Ix1 -> Ix2 -> Ix1 -> Ix1 -> m Bool
replaceAntInCell grid cellIx otherAntIx antIx =
  casIntArray grid cellIx otherAntIx antIx >>= \case
    Just prevIx -> pure (prevIx == otherAntIx)
    Nothing ->
      throwString $ "Impossible: Indexing out of bounds should not have happened: " <> show cellIx


tryToSwapWith :: Ant -> Ix2 -> Ant -> Ix2 -> RIO Env Bool
tryToSwapWith ant currentLocation otherAnt targetLocation =
  fromMaybe False <$> withAnt otherAnt swapAnts
  where
    swapAnts = do
      otherAntLocation <- readIORef $ antLocation otherAnt
      -- try swapping only 50% of the time
      shouldSwap <- randomBool
      -- make sure it is still the same "other" ant that we expect
      if otherAntLocation == targetLocation && shouldSwap
        then do
          logDebug $
            "swapping ants: " <> display (antIx otherAnt) <> " and " <> display (antIx ant)
          Colony {colonyGrid} <- envColony <$> ask
          unlessM (replaceAntInCell colonyGrid targetLocation (antIx otherAnt) (antIx ant)) $
            logError "Couldn't place an ant into a cell while swapping"
          writeIORef (antLocation ant) targetLocation
          unlessM (replaceAntInCell colonyGrid currentLocation (antIx ant) (antIx otherAnt)) $
            logError "Couldn't place another ant into the original cell while swapping"
          writeIORef (antLocation otherAnt) currentLocation
          pure True
        else pure False

isInsideNest :: Ant -> RIO Env (Maybe Nest)
isInsideNest Ant{antLocation} = do
  gridIx <- readIORef antLocation
  Grid {gridMap, gridNests} <- envGrid <$> ask
  cell <- gridMap !? gridIx
  pure $ lookupNest gridNests cell

startAssessing :: Nest -> Task
startAssessing nest@Nest {nestSize} = Assessing nest (AssessStepsLeft n) (Walk 0 0)
  where
    n = totalElem nestSize `div` 20

startConsidering :: Nest -> Task
startConsidering nest@Nest {nestSize, nestQuality} = Assessing nest (ConsiderStepsLeft k) (Walk 0 0)
  where
    k = totalElem nestSize `div` round (100 * nestQuality)

hasDiscovered :: MonadIO m => Ant -> Nest -> m Bool
hasDiscovered Ant {antDiscovered} Nest {nestIx} = do
  discoveredSet <- readIORef antDiscovered
  pure (nestIx `Set.member` discoveredSet)

startRecruiting :: Ant -> RIO Env Task
startRecruiting Ant {antDiscovered} = do
  discoveredSet <- readIORef antDiscovered
  Grid {gridNests} <- envGrid <$> ask
  discoveredNests <- P.mapM (indexM gridNests) $ F.toList discoveredSet
  let nest = F.maximumBy (\n1 n2 -> compare (nestQuality n1) (nestQuality n2)) discoveredNests
  pure $ Recruiting nest (Walk 0 0)


recruitAnt :: Ant -> Ant -> Task -> Maybe (Task, Ant)
recruitAnt leaderAnt followerAnt =
  let following = (TandemFollowing leaderAnt, followerAnt)
   in \case
        Searching {} -> Just following
        HangingAtHome {} -> Just following
        Recruiting {} -> Just following
        _ -> Nothing

performTask :: Ant -> Task -> RIO Env Task
performTask ant =
  \case
    Searching n walk ->
      isInsideNest ant >>= \case
        Nothing
          | n > 0 -> Searching (n - 1) <$> walkOutside ant walk
        Nothing -> do
          let hangAtHomeMaxSteps = 200
          k <- randomIntRange (10, hangAtHomeMaxSteps)
          homeNest <- askHomeNest
          HangingAtHome k <$> walkInside ant homeNest (Walk 0 0)
        Just nest -> do
          known <- hasDiscovered ant nest
          if known
            then Searching n <$> walkOutside ant walk
            else pure $ startAssessing nest
    HangingAtHome k walk -> do
      homeNest <- askHomeNest
      isInsideNest ant >>= \case
        Nothing
          | k > 0 -> HangingAtHome k <$> walkInside ant homeNest walk
        Nothing -> do
          let searchMaxSteps = 1000
          n <- randomIntRange (500, searchMaxSteps)
          Searching n <$> walkOutside ant (Walk 0 0)
        Just nest
          | nestIx nest == nestIx homeNest ->
            if k > 0
              then HangingAtHome (k - 1) <$> walkInside ant homeNest walk
              else HangingAtHome k <$> goOutside ant nest
        Just nest -> do
          known <- hasDiscovered ant nest
          if known
            then HangingAtHome k <$> goOutside ant nest
            else pure $ startAssessing nest
    Assessing nest (AssessStepsLeft n) walk ->
      isInsideNest ant >>= \case
        Nothing -> Assessing nest (AssessStepsLeft n) <$> walkInside ant nest walk
        Just nest'
          | nestIx nest' == nestIx nest ->
            if n > 0
              then Assessing nest (AssessStepsLeft (n - 1)) <$> walkInside ant nest walk
              else startConsidering nest <$
                   modifyIORef' (antDiscovered ant) (Set.insert (nestIx nest))
        Just nest' -> Assessing nest (AssessStepsLeft n) <$> goOutside ant nest'
    Assessing nest (ConsiderStepsLeft n) walk ->
      if n > 0
        then Assessing nest (ConsiderStepsLeft (n - 1)) <$> walkInside ant nest walk
        else startRecruiting ant
    Recruiting nest walk ->
      forNeighbors ant (\nAnt task -> pure (recruitAnt ant nAnt task)) >>= \case
        Just followerAnt -> pure $ TandemLeading nest (antIx followerAnt) (Walk 0 0)
        Nothing -> do
          homeNest <- askHomeNest
          Recruiting nest <$> walkInside ant homeNest walk
    TandemLeading nest fAntIx walk -> do
      mNewTask <-
        forNeighbors ant $ \nAnt task ->
          if antIx nAnt == fAntIx
            then isInsideNest ant >>= \case
                   Nothing -> do
                     walk' <- walkInside ant nest walk
                     pure $ Just (task, TandemLeading nest fAntIx walk')
                   Just nest'
                     | nestIx nest' == nestIx nest ->
                       if walkStepsLeft walk > 0
                         then do
                           walk' <- walkInside ant nest walk
                           pure $ Just (task, TandemLeading nest fAntIx walk')
                         else pure $ Just (startAssessing nest, Recruiting nest (Walk 0 0))
                     | otherwise -> do
                       walk' <- goOutside ant nest'
                       pure $ Just (task, TandemLeading nest fAntIx walk')
            else pure Nothing
      pure $ fromMaybe (TandemLeading nest fAntIx walk) mNewTask
    TandemFollowing lAnt -> do
      _ <-
        withAnt lAnt $ do
          currentLocation <- readIORef (antLocation ant)
          destination <- readIORef (antLocation lAnt)
          unless (destination `elem` cellNeighbors currentLocation) $
            case destinationToWalk currentLocation destination of
              Nothing -> pure ()
              Just destinationWalk -> void $ moveInDirection ant (walkDirection destinationWalk)
      pure $ TandemFollowing lAnt
    task -> pure task
      -- decideByTask (wTask -> Passive) =
      --   randomWalkInsideNest conf rGen track ant $
      --   if isJust $ aNewHome ant then fromJust $ aNewHome ant else wOldHome ant
      -- decideByTask (wTask -> (Searching n _)) =
      --   if n > 0
      --     then walkOutsideNests conf rGen track ant $ wDiscoveredNests ant
      --     else walkInsideNest conf rGen track ant $ aHome ant
      -- decideByTask (wTask -> (Assessing nest _)) =
      --   walkInsideNest conf rGen track ant nest
      -- decideByTask (wTask -> (Recruiting False)) =
      --   walkInsideNest conf rGen track ant $ aHome ant
      -- decideByTask (wTask -> (Recruiting True)) =
      --   walkInsideNest conf rGen track ant $ wOldHome ant
      -- decideByTask (wTask -> (TandemLeading ix')) = do
      --   ant' <- MV.read mColony ix'
      --   -- Move only whenever the follower is nearby
      --   if (fromJust $ aCurrent ant') `elem` (getNeighbors conf) (fromJust $ aCurrent ant)
      --     then moveInsideNest conf rGen track ant (fromJust $ aNewHome ant)
      --     else return ant { aNext = Nothing }
      -- decideByTask (wTask -> (TandemFollowing ix')) = do
      --   ant' <- MV.read mColony ix'
      --   moveToDestination conf rGen track $ ant {
      --     wDestination = Just (fromJust $ aCurrent ant', 0) }
      -- decideByTask (wTask -> (Transporting ix')) =
      --   if isNest curNest && -- if it is a nest
      --      (isNothing $ wDestination ant) && -- and transporter reached her destination
      --      curNest == (fromJust $ aNewHome ant) -- and destination is correct
      --   then do
      --     neighbors <- uniformShuffle
      --                  (V.fromList $ (getNeighbors conf) (fromJust $ aCurrent ant))
      --                  rGen
      --     let availableCells = V.filter (not . isTaken) neighbors
      --         isTaken sh = (isWall $ index (grid conf) sh) ||
      --                      ((index track sh) >= 0)
      --     if V.null availableCells
      --       then walkInsideNest conf rGen track ant curNest
      --       else do
      --       let shNext = Just $ V.head availableCells
      --       antTransported <- MV.read mColony ix'
      --       MV.write mColony ix' $ antTransported { aNext = shNext }
      --       return $ ant { aNext = shNext }
      --   else moveInsideNest conf rGen track ant (fromJust $ aNewHome ant)
      -- decideByTask (wTask -> (Transported _)) = return ant


colonyStep :: RIO Env ()
colonyStep = do
  Colony {colonyWorkers} <- envColony <$> ask
  A.forM_ colonyWorkers $ \worker ->
    withAntTask_ worker $ \task -> do
      task' <- performTask worker task
      Colony {colonyGrid} <- envColony <$> ask
      ix <- readIORef (antLocation worker)
      aix <- read' colonyGrid ix
      unless (aix == antIx worker) $
        logError $
        "Misplaced an ant. Expected ant: " <> display (antIx worker) <> " to be in cell: " <>
        displayShow ix
      pure task'

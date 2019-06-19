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
import RIO.Text as T
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


-- | Atomically modify an Ant. Returns `Nothing` if the Ant was busy and couldn't be
-- updated.
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

-- | Look up if there is an Ant at the specified index, and atomically modify that Ant
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

replaceAntInCell ::
     (MonadIO m, PrimMonad m) => MArray (PrimState m) P Ix2 Ix1
     -> Ix2 -- ^ Location where we want to place the ant
     -> Ix1 -- ^ Index of an ant that is expected to be in the cell
     -> Ix1 -- ^ Index of the ant that should be placed in that cell
     -> m Bool
replaceAntInCell grid cellIx otherAntIx antIx =
  casIntArray grid cellIx otherAntIx antIx >>= \case
    Just prevIx -> pure (prevIx == otherAntIx)
    Nothing ->
      throwString $ "Impossible: Indexing out of bounds should not have happened: " <> show cellIx


tryToSwapWith :: Ant -> Ix2 -> Ant -> Ix2 -> RIO Env Bool
tryToSwapWith ant currentLocation otherAnt targetLocation = do
  shouldSwap <- randomBool
  -- try swapping only 50% of the time
  if shouldSwap
    then fromMaybe False <$> withAnt otherAnt swapAnts
    else pure False
  where
    swapAnts = do
      otherAntLocation <- readIORef $ antLocation otherAnt
      -- make sure it is still the same "other" ant that we expect
      if otherAntLocation == targetLocation
        then do
          logDebug $ "swapping ants: " <> display (antIx otherAnt) <> " and " <> display (antIx ant)
          Colony {colonyGrid} <- envColony <$> ask
          unlessM (replaceAntInCell colonyGrid targetLocation (antIx otherAnt) (antIx ant)) $ do
            toPlace <- readIORef (antState ant)
            toRemove <- readIORef (antState otherAnt)
            logError $
              "Couldn't place an ant: " <> display (antIx ant) <>
              " into a cell while swapping with: " <>
              display (antIx otherAnt) <>
              " - " <>
              display toPlace <>
              ">>" <>
              display toRemove
          writeIORef (antLocation ant) targetLocation
          unlessM (replaceAntInCell colonyGrid currentLocation (antIx ant) (antIx otherAnt)) $ do
            toPlace <- readIORef (antState otherAnt)
            toRemove <- readIORef (antState ant)
            logError $
              "Couldn't place another ant: " <> display (antIx otherAnt) <>
              " into the original cell while swapping with: " <>
              display (antIx ant) <>
              " - " <>
              display toPlace <>
              ">>" <>
              display toRemove
          writeIORef (antLocation otherAnt) currentLocation
          pure True
        else pure False

isInsideNest :: Ant -> RIO Env (Maybe (Ix2, Nest))
isInsideNest Ant{antLocation} = do
  gridIx <- readIORef antLocation
  Grid {gridMap, gridNests} <- envGrid <$> ask
  cell <- gridMap !? gridIx
  pure ((gridIx, ) <$> lookupNest gridNests cell)

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

-- | Start recruiting to the best nest the ant has discovered thus far
startRecruiting :: Ant -> RIO Env Task
startRecruiting Ant {antDiscovered} = do
  discoveredSet <- readIORef antDiscovered
  Grid {gridNests} <- envGrid <$> ask
  discoveredNests <- P.mapM (indexM gridNests) $ F.toList discoveredSet
  let nest = F.maximumBy (\n1 n2 -> compare (nestQuality n1) (nestQuality n2)) discoveredNests
  pure $ Recruiting nest (Walk 0 0)


forNeighbors :: Ant -> (Ant -> Task -> RIO Env (Maybe (Task, a))) -> RIO Env (Maybe a)
forNeighbors Ant{antLocation} f = do
  ix <- readIORef antLocation
  foldM action Nothing $ cellNeighbors ix
  where
    action mAnt@Just {} _ = pure mAnt
    action Nothing cellIx = withAntAt cellIx f

recruitAnt :: Ant -> Ant -> Task -> Maybe (Task, Ant)
recruitAnt leaderAnt followerAnt targetAntTask =
  let following = Just (TandemFollowing targetAntTask leaderAnt, followerAnt)
   in case targetAntTask of
        Searching {} -> following
        HangingAtHome {} -> following
        Recruiting {} -> following
        _ -> Nothing

-- | Update Ant's task and remove her from the grid
pickupAnt :: Nest -> Ant -> Task -> RIO Env (Maybe (Task, Ant))
pickupAnt destinationNest targetAnt targetAntTask =
  let transportedAnt :: Task -> RIO Env (Maybe (Task, Ant))
      transportedAnt targetAntNextTask = do
        currentLocation <- readIORef (antLocation targetAnt)
        Colony {colonyGrid} <- envColony <$> ask
        removeAntFromCell colonyGrid currentLocation (antIx targetAnt)
        pure $ Just (Transported targetAntNextTask, targetAnt)
   in case targetAntTask of
        Searching {} -> transportedAnt targetAntTask
        HangingAtHome {} -> transportedAnt targetAntTask
        Recruiting toNest _
          | toNest /= destinationNest -> transportedAnt targetAntTask
        Passive homeNest _ _
          | homeNest /= destinationNest -> transportedAnt (Passive destinationNest 0 (Walk 0 0))
        _ -> pure Nothing

checkQuorum :: Nest -> RIO Env Bool
checkQuorum nest = do
  Colony {colonyWorkers} <- envColony <$> ask
  let countRecruiters acc Ant {antState} =
        stateTask <$> readIORef antState >>= \case
          Recruiting nest' _
            | nest' == nest -> pure (acc + 1)
          TandemLeading nest' _ _ _
            | nest' == nest -> pure (acc + 1)
          Transporting nest' _ _
            | nest' == nest -> pure (acc + 1)
          _ -> pure acc
  rCount <- A.foldlM countRecruiters 0 colonyWorkers
  pure (rCount >= totalElem (A.size colonyWorkers) `div` 10)

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
        Just (_, nest) -> do
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
          Grid {gridSearchSteps} <- envGrid <$> ask
          n <- randomIntRange (500, gridSearchSteps)
          Searching n <$> walkOutside ant (Walk 0 0)
        Just (_, nest)
          | nestIx nest == nestIx homeNest ->
            if k > 0
              then HangingAtHome (k - 1) <$> walkInside ant homeNest walk
              else HangingAtHome k <$> goOutside ant nest
        Just (_, nest) -> do
          known <- hasDiscovered ant nest
          if known
            then HangingAtHome k <$> goOutside ant nest
            else pure $ startAssessing nest
    Assessing nest (AssessStepsLeft n) walk ->
      isInsideNest ant >>= \case
        Nothing -> Assessing nest (AssessStepsLeft n) <$> walkInside ant nest walk
        Just (_, curNest)
          | curNest == nest ->
            if n > 0
              then Assessing nest (AssessStepsLeft (n - 1)) <$> walkInside ant nest walk
              else startConsidering nest <$
                   modifyIORef' (antDiscovered ant) (Set.insert (nestIx nest))
        Just (_, curNest) -> Assessing nest (AssessStepsLeft n) <$> goOutside ant curNest
    Assessing nest (ConsiderStepsLeft n) walk ->
      if n > 0
        then Assessing nest (ConsiderStepsLeft (n - 1)) <$> walkInside ant nest walk
        else startRecruiting ant
    Recruiting nest walk -> do
      homeNest <- askHomeNest
      isInsideNest ant >>= \case
        Just (_, curNest)
          | curNest /= homeNest -> do
            haveSeen <- hasDiscovered ant curNest
            if haveSeen
              then Recruiting nest <$> goOutside ant curNest
              else pure $ startAssessing nest
        _ -> do
          mNewTask <-
            forNeighbors ant $ \nAnt task -> do
              hasReachedQuorum <- checkQuorum nest
              if hasReachedQuorum
                then pickupAnt nest nAnt task >>= \case
                       Just (tTask, transportedAnt) ->
                         pure $ Just (tTask, Transporting nest transportedAnt (Walk 0 0))
                       Nothing -> pure Nothing
                else case recruitAnt ant nAnt task of
                       Just (fTask, followerAnt) ->
                         pure $ Just (fTask, TandemLeading nest followerAnt 0 (Walk 0 0))
                       Nothing -> pure Nothing
          case mNewTask of
            Just task -> pure task
            Nothing -> Recruiting nest <$> walkInside ant homeNest walk
    task@(TandemLeading nest fAnt waitingSteps _)
      | waitingSteps > 50 -> do
        isLost <-
          withAntTask_ fAnt $ \case
            TandemFollowing prevTask lAnt
              | ant == lAnt -> pure prevTask
            fTask ->
              throwString $ "Follower is in an impossible state: " ++ T.unpack (textDisplay fTask)
        if isLost
          then pure $ Recruiting nest (Walk 0 0)
          else pure task
    task@(TandemLeading nest fAnt waitingSteps walk) ->
      fmap (fromMaybe task) $
      forNeighbors ant $ \nAnt nTask ->
        if nAnt == fAnt -- only move when the follower is nearby
          then isInsideNest ant >>= \case
                 Just (curIx, curNest)
                   | curNest == nest && isCloseToCenter curIx nest ->
                     pure $ Just (startAssessing nest, Recruiting nest (Walk 0 0))
                 Just (_curIx, curNest)
                   | curNest == nest -> do
                     walk' <- walkInside ant nest walk
                     pure $ Just (nTask, TandemLeading nest fAnt 0 walk')
                 Nothing -> do
                   walk' <- walkInside ant nest walk
                   pure $ Just (nTask, TandemLeading nest fAnt 0 walk')
                 Just (_curIx, curNest) -> do
                   walk' <- goOutside ant curNest
                   pure $ Just (nTask, TandemLeading nest fAnt 0 walk')
          else pure $ Just (nTask, TandemLeading nest fAnt (waitingSteps + 1) walk)
    task@(TandemFollowing _ lAnt) -> do
      _ <-
        withAnt lAnt $ do
          currentLocation <- readIORef (antLocation ant)
          destination <- readIORef (antLocation lAnt)
          unless (destination `elem` cellNeighbors currentLocation) $
            case destinationToWalk currentLocation destination of
              Nothing -> pure ()
              Just destinationWalk -> void $ moveInDirection ant (walkDirection destinationWalk)
      pure task
    task@(Transporting targetNest transportedAnt walk) ->
      let updateTransported tCurTask newWalk = do
            newLocIx <- readIORef (antLocation ant)
            writeIORef (antLocation transportedAnt) newLocIx
            pure (tCurTask, Transporting targetNest transportedAnt newWalk)
       in fmap (fromMaybe task) $
          withAntTask transportedAnt $ \tCurTask ->
            isInsideNest ant >>= \case
              Just (curIx, curNest)
                | curNest == targetNest && isCloseToCenter curIx targetNest -> do
                  newWalk <- walkInside ant targetNest walk
                  -- Here we know that transporter has moved out from the cell so we can
                  -- safely drop the transported one here.
                  Colony {colonyGrid} <- envColony <$> ask
                  placeAntInCell colonyGrid curIx (antIx transportedAnt) >>= \case
                    HasPlaced
                      | Transported tPrevTask <- tCurTask ->
                        pure (tPrevTask, Recruiting targetNest newWalk)
                    HasOccupant _ -- some other ant took the spot, abort the drop
                     -> updateTransported tCurTask newWalk
                    res ->
                      throwString $
                      "Dropping off ant has failed for unexpected reason: " <> show res
                | curNest == targetNest ->
                  updateTransported tCurTask =<< walkInside ant targetNest walk
                | otherwise -> updateTransported tCurTask =<< goOutside ant curNest
              Nothing -> updateTransported tCurTask =<< walkInside ant targetNest walk
    task@Transported {} -> pure task
    Passive nest step walk
      | step > 0 -> pure $ Passive nest (step - 1) walk
    Passive nest _ walk -> do
      stepsBeforeNextMove <- randomIntRange (3, 15)
      Passive nest stepsBeforeNextMove <$> walkInside ant nest walk


isCloseToCenter :: Ix2 -> Nest -> Bool
isCloseToCenter curIx Nest {..} = isSafeIndex (Sz (2 * forthSz)) (curIx - (nestNorthWest + forthSz))
  where
    forthSz = liftIndex (`div` 4) (unSz nestSize)


colonyStep :: RIO Env ()
colonyStep = do
  Colony {colonyWorkers} <- envColony <$> ask
  A.forIO_ colonyWorkers $ \worker ->
    withAntTask_ worker $ \task -> do
      task' <- performTask worker task
      case task' of
        Transported {} -> pure ()
        _ -> do
          Colony {colonyGrid} <- envColony <$> ask
          ix <- readIORef (antLocation worker)
          aix <- read' colonyGrid ix
          unless (aix == antIx worker) $
            logError $
              "Misplaced an ant. Expected ant: " <> display (antIx worker) <>
              " while switching from: " <> display task <>
              " to task: " <>
              display task' <>
              " to be in cell: " <>
              displayShow ix
      pure task'

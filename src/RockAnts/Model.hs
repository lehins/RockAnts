{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RockAnts.Model where


import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Atomic
import RIO as P
import RockAnts.Grid
import RockAnts.Types
import RockAnts.Random
import System.Random.MWC

getHomeNest :: RIO Env Nest
getHomeNest = homeNest . envGrid <$> ask

chooseDestination :: (HasGen env, Source r Ix1 Ix2) => Ant -> Array r Ix1 Ix2 -> RIO env ()
chooseDestination Ant {antDestination} arr = randomElement arr >>= writeIORef antDestination



searchOutside :: Ant -> Walk -> RIO Env Walk
searchOutside ant@Ant {..} walk = go
  where
    go = do
      logDebug "Searching outside"
      mDestination <- readIORef antDestination
      case mDestination of
        Just destination -> do
          currentLocation <- readIORef antLocation
          case destinationToDirection currentLocation destination of
            Nothing -> do
              writeIORef antDestination Nothing
              go
            Just direction -> walk <$ moveInDirection ant direction
        Nothing -> do
          Grid {gridNests, gridMap} <- envGrid <$> ask
          gridIx <- readIORef antLocation
          case gridMap !? gridIx >>= lookupNest gridNests of
            Nothing -> do
              Walk {..} <- maybeChangeWalkDirection walk
              adjustedDirection <- moveInDirection ant walkDirection
              pure Walk {walkStepsLeft = walkStepsLeft - 1, walkDirection = adjustedDirection}
            Just nest -> do
              chooseDestination ant $ nestEntranceRange nest
              go

maybeChangeWalkDirection :: Walk -> RIO Env Walk
maybeChangeWalkDirection walk@Walk {..}
  | walkStepsLeft > 0 = pure walk
  | otherwise = do
    newDirection <- randomDirection
    Env {envGen, envConstants} <- ask
    numSteps <- uniformR (0, constMaxSteps envConstants) envGen -- (0, maxSteps]
    pure Walk {walkDirection = newDirection, walkStepsLeft = numSteps}

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
      direction <$ writeIORef antLocation targetLocation -- TODO: adjust direction due to pi/3 movement
    HitWall -> adjustDirection direction >>= moveInDirection ant
    HasOccupant occupantIx -> do
      occupant <- indexM colonyAnts occupantIx
      hasSwapped <- tryToSwapWith ant currentLocation occupant targetLocation
      if hasSwapped
        then pure direction
        else adjustDirection direction >>= moveInDirection ant

adjustDirection :: Double -> RIO Env Double
adjustDirection direction = do
  Env {envGen} <- ask
  turnLeft <- uniform envGen
  pure $
    if turnLeft
      then direction + pi / 3
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


replaceAntInCell :: PrimMonad m => MArray (PrimState m) P Ix2 Ix1 -> Ix2 -> Ix1 -> Ix1 -> m Bool
replaceAntInCell grid cellIx otherAntIx antIx = isJust <$> casIntArray grid cellIx otherAntIx antIx


tryToSwapWith :: Ant -> Ix2 -> Ant -> Ix2 -> RIO Env Bool
tryToSwapWith ant currentLocation otherAnt targetLocation =
  fromMaybe False <$> withAnt otherAnt swapAnts
  where
    swapAnts = do
      otherAntLocation <- readIORef $ antLocation otherAnt
      -- make sure it is still the same "other" ant that we expect
      if otherAntLocation == targetLocation
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



--randomWalk ant@Ant {..} = undefined

-- -- | A random walk, which is something like a Brownian motion in 2D, but oversimplified.
-- randomWalk ant@(Ant { wDirection = dir }) =
--   forM (getWalk task) $ \ Walk {..} -> do
--     if isJust dir && momentum > 0 && aHasMoved ant
--       then do err <- uniformExclusiveR rGen (-mav, mav)
--               return $ (setDirection conf ant (direction + err) (momentum - 1)) {
--                 wNextPriority = priority }
--       else do
--       constants <- configConstants <$> ask
--       momentum' <- uniformR (cMmr $ constants conf) rGen
--               direction' <- (2*pi*) <$> uniform' rGen -- [0, 2pi)
--               return $ (setDirection conf ant direction' momentum') {
--                 wNextPriority = priority }


-- walkInsideNest ant@Ant{..} Nest {..} = do
--   undefined

performTask :: Ant -> Task -> RIO Env Task
performTask ant@Ant {..} =
  \case
    (Searching n m walk) ->
      if n > 0
        then Searching (n - 1) m <$> searchOutside ant walk
        else Searching 20 m <$> searchOutside ant walk --getHomeNest >>= walkInsideNest ant
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

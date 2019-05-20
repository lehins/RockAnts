{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RockAnts.Model where


import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Massiv.Array.Mutable.Atomic
import RIO as P
import RockAnts.Grid
import RockAnts.Types
import RockAnts.Random

getHomeNest :: RIO Env Nest
getHomeNest = homeNest . envGrid <$> ask

chooseDestination :: (HasGen env, Source r Ix1 Ix2) => Ant -> Array r Ix1 Ix2 -> RIO env ()
chooseDestination Ant {antDestination} arr = randomElement arr >>= writeIORef antDestination



searchOutside :: Ant -> Walk -> RIO Env Walk
searchOutside ant@Ant {..} walk = go
  where
    go = do
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

maybeChangeWalkDirection walk@Walk {..}
  | walkStepsLeft > 0 = pure walk
  | otherwise = do
      pure walk

moveInDirection :: Ant -> Double -> RIO Env Double
moveInDirection ant@Ant {..} direction = do
  currentLocation <- readIORef antLocation
  -- Cell index where an attempt to move will happen.
  let cellIx = cellInDirection currentLocation direction
  grid <- colonyGrid . envColony <$> ask
  hasMoved <- placeAntInCell grid cellIx antIx
  if hasMoved
    then direction <$ writeIORef antLocation cellIx
    else read' grid cellIx >>= \case
           occupant
             | Cell occupant == wallCell -> adjustDirection
             | Cell occupant == emptyCell -> moveInDirection ant direction
             | otherwise -> do
               tryToSwapWith ant occupant
               pure direction

adjustDirection = undefined

tryToSwapWith = undefined

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

-- updateNextState :: Ant -> State -> RIO Env (Maybe State)
-- updateNextState ant@Ant {..} = \case
--   Busy {} -> pure Nothing
--   Idle task ->
--     case task of
--       Searching n _ ->
--         if n > 0 || True
--           then walkOutside ant
--           else getHomeNest >>= walkInsideNest ant
--       _ -> pure $ Just $ Idle task
--       -- decideByTask (wTask -> Passive) =
--       --   randomWalkInsideNest conf rGen track ant $
--       --   if isJust $ aNewHome ant then fromJust $ aNewHome ant else wOldHome ant
--       -- decideByTask (wTask -> (Searching n _)) =
--       --   if n > 0
--       --     then walkOutsideNests conf rGen track ant $ wDiscoveredNests ant
--       --     else walkInsideNest conf rGen track ant $ aHome ant
--       -- decideByTask (wTask -> (Assessing nest _)) =
--       --   walkInsideNest conf rGen track ant nest
--       -- decideByTask (wTask -> (Recruiting False)) =
--       --   walkInsideNest conf rGen track ant $ aHome ant
--       -- decideByTask (wTask -> (Recruiting True)) =
--       --   walkInsideNest conf rGen track ant $ wOldHome ant
--       -- decideByTask (wTask -> (TandemLeading ix')) = do
--       --   ant' <- MV.read mColony ix'
--       --   -- Move only whenever the follower is nearby
--       --   if (fromJust $ aCurrent ant') `elem` (getNeighbors conf) (fromJust $ aCurrent ant)
--       --     then moveInsideNest conf rGen track ant (fromJust $ aNewHome ant)
--       --     else return ant { aNext = Nothing }
--       -- decideByTask (wTask -> (TandemFollowing ix')) = do
--       --   ant' <- MV.read mColony ix'
--       --   moveToDestination conf rGen track $ ant {
--       --     wDestination = Just (fromJust $ aCurrent ant', 0) }
--       -- decideByTask (wTask -> (Transporting ix')) =
--       --   if isNest curNest && -- if it is a nest
--       --      (isNothing $ wDestination ant) && -- and transporter reached her destination
--       --      curNest == (fromJust $ aNewHome ant) -- and destination is correct
--       --   then do
--       --     neighbors <- uniformShuffle
--       --                  (V.fromList $ (getNeighbors conf) (fromJust $ aCurrent ant))
--       --                  rGen
--       --     let availableCells = V.filter (not . isTaken) neighbors
--       --         isTaken sh = (isWall $ index (grid conf) sh) ||
--       --                      ((index track sh) >= 0)
--       --     if V.null availableCells
--       --       then walkInsideNest conf rGen track ant curNest
--       --       else do
--       --       let shNext = Just $ V.head availableCells
--       --       antTransported <- MV.read mColony ix'
--       --       MV.write mColony ix' $ antTransported { aNext = shNext }
--       --       return $ ant { aNext = shNext }
--       --   else moveInsideNest conf rGen track ant (fromJust $ aNewHome ant)
--       -- decideByTask (wTask -> (Transported _)) = return ant


-- colonyStep :: RIO Env ()
-- colonyStep = do
--   Colony {colonyWorkers} <- envColony <$> ask
--   A.forM_ colonyWorkers $ \ worker@Ant {..} -> do
--     currentState <- atomicModifyIORef' antState $ \case
--       state@(Idle task) -> (Busy task, state)
--       state -> (state, state)
--     mNewState <- updateNextState worker currentState
--     P.forM_ mNewState (writeIORef antState)

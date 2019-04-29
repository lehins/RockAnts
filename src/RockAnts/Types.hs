{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module RockAnts.Types where

import Data.Coerce
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Massiv.Array.Mutable.Atomic
import Data.Primitive.MutVar
import Graphics.ColorSpace
import RIO
import System.Random.MWC

-- | -2: wall, -1: empty, n: Nest index
newtype Cell =
  Cell Ix1
  deriving (Eq, NFData, Prim)

emptyCell :: Cell
emptyCell = Cell (-1)

wallCell :: Cell
wallCell = Cell (-2)

lookupNest :: Array B NestIx Nest -> Cell -> Maybe Nest
lookupNest arr cell@(Cell ix)
  | cell == emptyCell || cell == wallCell = Nothing
   -- index out of bounds would indicate a bug, be loud about it:
  | otherwise = Just $ index' arr ix


instance Show Cell where
  show cell@(Cell ix)
    | cell == emptyCell = "_"
    | cell == wallCell = "W"
    | otherwise = show ix

data Nest = Nest
  { nestScore         :: !Float
  , nestSize          :: !Sz2
    -- ^ Overall size of the nest
  , nestNorthWest     :: !Ix2
    -- ^ An index for the top left corner of the nest
  , nestEntranceStart :: !Ix2
  , nestEntranceEnd   :: !Ix2
  } deriving (Eq, Show)

type NestIx = Ix1

data IsMoving = Moving | NotMoving

data WorkerState
  = Passive
    -- | Timesteps before going to OldHome and outside of it.
  | Searching !Int !(Maybe Int)
    -- | Number of steps left to evaluate a nest
  | Assessing !NestIx !Int
    -- | After Assessing takes some time to consider new nest
  | Recruiting !Bool
    -- | Ant index Ix1 is leading
  | TandemLeading !Int
    -- | Ant index she is following
  | TandemFollowing !Ix1
    -- | Ant index she is transporting
  | Transporting !Int
    -- | Transported worker needs to remember what she was doing
  | Transported !WorkerState

data AntType
  = Worker
  | Brood
  | Queen
  deriving (Eq, Show)

data Ant = Ant
  { antDestination   :: !(IORef (Maybe Ix2))
  , antLocation      :: !(IORef Ix2)
  , antState         :: !(IORef WorkerState)
  , antIsMovingBaton :: !(IORef IsMoving)
  , antType          :: !AntType
  -- ^ Set to True whenever a worker is: carrying, scouting, tandem leading, being lead
  }

-- takeWorkerIsMovingBaton :: PrimMonad m => Worker m -> m IsMoving
-- takeWorkerIsMovingBaton Worker {workerIsMovingBaton} =
--   atomicModifyMutVar' workerIsMovingBaton $ \ movingBaton -> (movingBaton, Moving)

data Colony = Colony
  { colonyGrid    :: !(MArray (PrimState IO) P Ix2 Ix1)
  , colonyAnts    :: !(Array B Ix1 Ant)
  , colonyWorkers :: !(Array M Ix1 Ant)
  , colonyGen     :: !(Gen (PrimState IO))
  }

data Grid = Grid
  { gridNests :: !(Array B Ix1 Nest)
  -- , gridColony      :: Array B Ix1 (Ant m)
  -- , gridHomeNestIx  :: NestIx
  , gridMap   :: !(Array P Ix2 Cell)
  , gridImage :: !(Image S RGB Word8)
  -- , gridCurrentAnts :: MArray (PrimState m) P Ix2 Ix1
  -- -- / starts as gridArrayAnts frozen
  -- , gridNextAnts    :: MArray (PrimState m) P Ix2 Ix1
  }

getHomeNest :: Grid -> Nest
getHomeNest Grid {gridNests} =
  case gridNests !? 0 of
    Nothing   -> error "Home nest has not been initialized"
    Just home -> home

placeAntIn :: MArray (PrimState IO) P Ix2 Ix1 -> Ix2 -> Ix1 -> IO Bool
placeAntIn grid nestIx antIx = isJust <$> casIntArray grid nestIx (coerce emptyCell) antIx

getNestIndices :: Nest -> Array U Ix1 Ix2
getNestIndices nest =
  computeAs U $ flatten (nestNorthWest nest + 1 ... nestNorthWest nest + unSz (nestSize nest) - 1)

newColony :: Seed -> Grid -> Sz1 -> Sz1 -> IO Colony
newColony colonySeed grid@Grid {gridMap} workerCount broodCount = do
  colonyGen <- restore colonySeed
  colonyGrid <- loadArrayS $ A.map clearNestsIndices gridMap
  antTypes <-
    concatM 1
      [ A.replicate Seq workerCount Worker
      , A.replicate Seq broodCount Brood
      , A.replicate Seq queenCount Queen :: Array D Ix1 AntType
      ]
  let homeIndices = getNestIndices (getHomeNest grid)
      getAvailableSpotFor antIx = do
        i <- uniformR (0, unSz (size homeIndices) - 1) colonyGen
        ix <- indexM homeIndices i
        placed <- placeAntIn colonyGrid ix antIx
        if placed
          then pure ix
          else getAvailableSpotFor antIx
      initAnt antIx antType = do
        antDestination <- newIORef Nothing
        locIx <- getAvailableSpotFor antIx
        antLocation <- newIORef locIx
        antState <- newIORef Passive
        antIsMovingBaton <- newIORef NotMoving
        pure Ant {..}
  colonyAnts <- itraversePrim initAnt $ computeAs B antTypes
  colonyWorkers <- extractM 0 workerCount colonyAnts
  pure Colony {..}
  where
    queenCount = 1
    clearNestsIndices (Cell ix) =
      if ix >= 0
        then 0
        else ix


-- Each ant:to:ant interaction should be:
--  * Check for expected state
--  * Try take baton
--  * Check for the same state again

-- Ant taking a step:
--  * Two atomic writes
--    0. set ant as moving (try take baton, if failed it is being carried)
--    1. CAS: if destination cell is empty or self, but not a wall or another ant, move that ant there
--    2. upon succeess remove that ant from previous cell with a simple write -1
--    3. update his location in self
--    4. in above situation, an ant will be in two cells. Why is it not a problem:
--       * no ant can go into a cell where another ant is
--       * In order for ant to be picked up, he has to be non-moving
--    5. set ant as non-moving

-- Ant picking up another ant
--  * Before can carry self baton needs to be picked up
--  * Whenever ant is looking to carry another and finds one
--    0. try take his moving baton, if not just continue with moving
--    1. pick her up: take her of the grid
--    2. set busy flag (carrying, scouting, tandem leading, being lead)
--    2. keep updating her location as well as self for image rendering
--    3. do not release either of the batons until drop off
--

-- Deciding on leading another ant
--  * Before can lead self baton needs to be picked up
--  * Whenever an ant is near by
--    0. try take his baton, if not just continue with recruitement
--    1. update both ants state
--    2. release followers baton (she can't be bothered in a follower state)
--    3. release self baton


-- Leading
--  0. Take self baton
--  1. take a step
--  2. do not release baton

-- Follower
--  0. Take self baton
--  1. Make a step towards the leader
--  2. If leader is in the nearby cell, release her baton



-- Facts:
--
--  * T. albipennis scouts show behavioural lateralization when exploring unknown nest sites,
--  showing a population-level bias to prefer left turns. One possible reason for this is that its
--  environment is partly maze-like and consistently turning in one direction is a good way to
--  search and exit mazes without getting lost.

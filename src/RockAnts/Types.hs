{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module RockAnts.Types where

import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Primitive.MutVar
import Graphics.ColorSpace
import RIO



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


data IsMoving = Moving | NotMoving

data WorkerStatus
  = Passive
    -- | Timesteps before going to OldHome and outside of it.
  | Searching !Int !(Maybe Int)
    -- | Number of steps left to evaluate a nest
  | Assessing !NestIx !Int
    -- | After Assessing takes some time to consider new nest
  | Recruiting !Bool
    -- | Ant index she is leading
  | TandemLeading !Int
    -- | Ant index she is following
  | TandemFollowing !Int
    -- | Ant index she is transporting
  | Transporting !Int
    -- | Transported worker needs to remember what she was doing
  | Transported !WorkerStatus

data Worker m = Worker
  { workerDestination   :: MutVar (PrimState m) Ix2
  , workerLocation      :: MutVar (PrimState m) Ix2
  , workerBusy          :: MutVar (PrimState m) WorkerStatus
  , workerIsMovingBaton :: MutVar (PrimState m) IsMoving
  -- ^ Set to True whenever a worker is: carrying, scouting, tandem leading, being lead
  }

takeWorkerIsMovingBaton :: PrimMonad m => Worker m -> m IsMoving
takeWorkerIsMovingBaton Worker {workerIsMovingBaton} =
  atomicModifyMutVar' workerIsMovingBaton $ \ movingBaton -> (movingBaton, Moving)

data Brood

data Queen

type NestIx = Ix1

data Ant m
  = AntWorker (Worker m)
  | AntBrood Brood
  | AntQueen Queen

data RockAnt = RockAnt
  { rockAntGridLocation :: MVar Ix2
  , rockAntInsideNest   :: NestIx
  }


data Grid = Grid
  { gridNests :: Array B NestIx Nest
  -- , gridColony      :: Array B Ix1 (Ant m)
  -- , gridHomeNestIx  :: NestIx
  , gridMap   :: Array P Ix2 Cell
  , gridImage :: Image S RGB Word8
  -- , gridCurrentAnts :: MArray (PrimState m) P Ix2 Ix1
  -- -- / starts as gridArrayAnts frozen
  -- , gridNextAnts    :: MArray (PrimState m) P Ix2 Ix1
  }

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


-- data Cell
--   = Ant RockAnt
--   | Wall
--   | Empty


-- Facts:
--
--  * T. albipennis scouts show behavioural lateralization when exploring unknown nest sites,
--  showing a population-level bias to prefer left turns. One possible reason for this is that its
--  environment is partly maze-like and consistently turning in one direction is a good way to
--  search and exit mazes without getting lost.

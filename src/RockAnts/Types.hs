{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module RockAnts.Types where

import Data.Coerce
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Massiv.Array.Mutable.Atomic
import Graphics.ColorSpace
import RIO
import RIO.Set as Set
import RockAnts.Random
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
  { nestIx            :: !Ix1
  , nestScore         :: !Float
  , nestSize          :: !Sz2
    -- ^ Overall size of the nest
  , nestNorthWest     :: !Ix2
    -- ^ An index for the top left corner of the nest
  , nestEntranceStart :: !Ix2
  , nestEntranceEnd   :: !Ix2
  } deriving (Eq, Show)

nestEntranceRange :: Nest -> Array D Ix1 Ix2
nestEntranceRange Nest {nestEntranceStart, nestEntranceEnd} =
  flatten (nestEntranceStart ... nestEntranceEnd)


type NestIx = Ix1


data Walk = Walk
  { walkDirection :: !Double
  , walkStepsLeft :: !Int
  }

data Task
  = -- | Timesteps before going to OldHome and outside of it.
  Searching !Int !(Maybe Int) !Walk
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
  | Transported !Task
  | Passive

getWalk :: Task -> Maybe Walk
getWalk = \case
  Searching _ _ walk -> Just walk
  _                  -> Nothing

stateTask :: State -> Task
stateTask (Idle task) = task
stateTask (Busy task) = task

data State =
    Idle !Task
  | Busy !Task

data AntType
  = Worker
  | Brood
  | Queen
  deriving (Eq, Show)

data Ant = Ant
  { antIx          :: !Ix1
  , antDestination :: !(IORef (Maybe Ix2))
  , antLocation    :: !(IORef Ix2)
  , antState       :: !(IORef State)
  , antType        :: !AntType
  , antDiscovered  :: !(Set NestIx)
  }

data Colony = Colony
  { colonyGrid    :: !(MArray (PrimState IO) P Ix2 Ix1)
  , colonyAnts    :: !(Array B Ix1 Ant)
  , colonyWorkers :: !(Array M Ix1 Ant)
  }

data CellDrawer = CellScaler
  { csCellSize   :: !Sz2
  , csCellOffset :: !Ix2
  , csDrawCell   :: forall m a. Monad m => (Ix2 -> m a) -> Ix2 -> m ()
  }

data Grid = Grid
  { gridNests      :: !(Array B Ix1 Nest)
  , gridMap        :: !(Array P Ix2 Cell)
  , gridImage      :: !(Image S RGB Word8)
  , gridCellDrawer :: !CellDrawer
  }


data Env = Env
  { envColony    :: !Colony
  -- ^ Live representatives of a colony. This is the mutable part of the environment
  , envGrid      :: !Grid
  -- ^ The immutable part of the environment. A grid with a map of non-moving elements
  -- such as walls of nests, as well as a way to draw on it.
  , envLogFunc   :: !LogFunc
  , envGen       :: !(Gen (PrimState IO))
  , envConstants :: !Constants
  }

instance HasGen Env where
  genG = to envGen

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\ e f -> e { envLogFunc = f })

homeNest :: Grid -> Nest
homeNest Grid {gridNests} =
  fromMaybe (error "Home nest has not been initialized") (gridNests !? 0)

data PlaceResult
  = HasPlaced
  | HitWall
  | HasOccupant Ix1

placeAntInCell ::
     (MonadIO m, PrimMonad m) => MArray (PrimState m) P Ix2 Ix1 -> Ix2 -> Ix1 -> m PlaceResult
placeAntInCell grid cellIx antIx =
  casIntArray grid cellIx (coerce emptyCell) antIx >>= \case
    Nothing -> throwString $
               "Should not attempt to write into out of bounds cell: " <> show cellIx
    Just prevIx | Cell prevIx == emptyCell -> pure HasPlaced
                | Cell prevIx == wallCell -> pure HitWall
                | otherwise -> pure $ HasOccupant prevIx

getNestIndices :: Nest -> Array U Ix1 Ix2
getNestIndices nest =
  computeAs U $ flatten (nestNorthWest nest + 1 ... nestNorthWest nest + unSz (nestSize nest) - 1)


data GridScale
  = GridScale2x2
  | GridScale3x4
  | GridScale5x6
  deriving (Eq, Show)

data GridSpec = GridSpec
  { gridSpecSize  :: !Sz2
  , gridSpecScale :: !GridScale
  , gridSpecNests :: !(Array B Ix1 Nest)
  } deriving Show

data Config = Config
  { configSeed      :: !Seed
  , configGridSpec  :: !GridSpec
  , configWorkers   :: !Sz1
  , configBrood     :: !Sz1
  , configSteps     :: !(Maybe Int)
  -- ^ Number of steps to terminate simulation after if a new nest doesn't get found
  , configVerbose   :: !Bool
  , configConstants :: !Constants
  , configMaxSteps  :: !Int
  -- ^ What is the maximum number of steps to run the model for until consider run as failed
  }

data Constants = Constants
  { constPpm      :: !Double -- ^ Probability a Passive ant moves
  , constWms      :: !Int -- ^ Random walk max steps
  , constMav      :: !Double -- ^ Movement angle variance
  , constAts      :: !Int -- ^ Assess for time steps
  , constPap      :: !Double
  -- ^ Probability an ant becomes Passive after assessment. Takes nest's score into
  -- account as well
  , constMaxSteps :: !Int
  }


newColony :: Gen (PrimState IO) -> Grid -> Sz1 -> Sz1 -> IO Colony
newColony colonyGen grid@Grid {gridMap} workerCount broodCount = do
  colonyGrid <- loadArrayS $ A.map clearNestsIndices gridMap
  antTypes <-
    concatM
      1
      [ A.replicate Seq workerCount Worker
      , A.replicate Seq broodCount Brood
      , A.replicate Seq queenCount Queen :: Array D Ix1 AntType
      ]
  let homeIndices = getNestIndices (homeNest grid)
      -- if home is smaller than number of ants, this will loop forever
      getAvailableSpotFor antIx = do
        i <- uniformR (0, unSz (A.size homeIndices) - 1) colonyGen
        ix <- indexM homeIndices i
        placeAntInCell colonyGrid ix antIx >>= \case
          HasPlaced -> pure ix
          _ -> getAvailableSpotFor antIx
      antDiscovered = Set.singleton 0
      initAnt antIx antType = do
        antDestination <- newIORef Nothing
        locIx <- getAvailableSpotFor antIx
        antLocation <- newIORef locIx
        antState <- newIORef $ Idle Passive
        pure Ant {..}
  colonyAnts <- itraversePrim initAnt $ computeAs B antTypes
  colonyWorkers <- extractM 0 workerCount colonyAnts
  pure Colony {..}
  where
    queenCount = 1
    clearNestsIndices (Cell ix) =
      if ix >= 0
        then coerce emptyCell
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

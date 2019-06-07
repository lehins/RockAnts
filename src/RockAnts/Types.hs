{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
import System.Random.SplitMix

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
  , nestQuality       :: !Float
  , nestSize          :: !Sz2
    -- ^ Overall size of the nest
  , nestNorthWest     :: !Ix2
    -- ^ An index for the top left corner of the nest
  , nestEntranceStart :: !Ix2
  , nestEntranceEnd   :: !Ix2
  } deriving Show

instance Eq Nest where
  n1 == n2 = nestIx n1 == nestIx n2

nestEntranceRange :: Nest -> Array D Ix1 Ix2
nestEntranceRange Nest {nestEntranceStart, nestEntranceEnd} =
  flatten (nestEntranceStart ... nestEntranceEnd)


type NestIx = Ix1


data Walk = Walk
  { walkDirection :: !Double
  , walkStepsLeft :: !Int
  }

instance Display Walk where
  display Walk {..} = "Walk <dir=" <> display walkDirection <> ",m=" <> display walkStepsLeft <> ">"

data AssessSteps
  = AssessStepsLeft !Int
  | ConsiderStepsLeft !Int

data Task
  = -- | Timesteps before going to OldHome and outside of it.
  Searching !Int !Walk
    -- | Once every so often scouts go home for a while
  | HangingAtHome !Int !Walk
    -- | Number of steps left to evaluate a nest
  | Assessing !Nest !AssessSteps !Walk
    -- | After Assessing an ant will start recruiting. Once quorum is reached,
    -- transportation recruitement starts.
  | Recruiting !Nest !Walk
    -- | Contains an Ant that she is leading, nest where to adn number of steps since
    -- last move, in case follower is lost
  | TandemLeading !Nest !Ant !Int !Walk
    -- | Contains an Ant she is following and a previous task, in case she gets lost
  | TandemFollowing !Task !Ant
    -- | Ant index she is transporting
  | Transporting !Nest !Ant !Walk
    -- | Transported worker needs to remember what she was doing
  | Transported !Task
  | Passive !Nest !Int !Walk

stateTask :: State -> Task
stateTask (Idle task) = task
stateTask (Busy task) = task

instance Display State where
  display (Idle t) = "Idle(" <> display t <> ")"
  display (Busy t) = "Busy(" <> display t <> ")"

instance Display Task where
  display =
    \case
      Searching s _w -> "Searching s:" <> display s
      HangingAtHome s _w -> "HangingAtHome s:" <> display s
      Assessing n _s _w -> "Assessing nest:" <> display (nestIx n)
      Recruiting n _w -> "Recruiting nest:" <> display (nestIx n)
      TandemLeading n a _ _w ->
        "TandemLeading nest:" <> display (nestIx n) <> " ant:" <> display (antIx a)
      TandemFollowing _ a -> "TandemFollowing ant:" <> display (antIx a)
      Transporting n a _w ->
        "Transporting nest:" <> display (nestIx n) <> " ant:" <> display (antIx a)
      Transported t -> "Transported task: <" <> display t <> ">"
      Passive n _s _w -> "Passive nest:" <> display (nestIx n)

data State =
    Idle !Task
  | Busy !Task

data AntType
  = Worker
  | Brood
  | Queen
  deriving (Eq, Show)

data Ant = Ant
  { antIx         :: !Ix1
  , antLocation   :: !(IORef Ix2)
  , antState      :: !(IORef State)
  , antType       :: !AntType
  , antDiscovered :: !(IORef (Set NestIx))
  }

-- | Ant index is her identifier.
instance Eq Ant where
  a1 == a2 = antIx a1 == antIx a2

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
  { gridNests       :: !(Array B Ix1 Nest)
  , gridMap         :: !(Array P Ix2 Cell)
  , gridImage       :: !(Image S RGB Word8)
  , gridCellDrawer  :: !CellDrawer
  , gridSearchSteps :: !Int
  -- ^ Max number of steps, before a scount should go home. It seems like it should be
  -- proportional to the size of the environment
  }


data Env = Env
  { envColony    :: !Colony
  -- ^ Live representatives of a colony. This is the mutable part of the environment
  , envGrid      :: !Grid
  -- ^ The immutable part of the environment. A grid with a map of non-moving elements
  -- such as walls of nests, as well as a way to draw on it.
  , envLogFunc   :: !LogFunc
  , envGen       :: !(IORef SMGen)
  , envConstants :: !Constants
  }

instance HasGen Env where
  genG = to envGen

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\ e f -> e { envLogFunc = f })

getHomeNest :: Grid -> Nest
getHomeNest Grid {gridNests} =
  fromMaybe (error "Home nest has not been initialized") (gridNests !? 0)

askHomeNest :: RIO Env Nest
askHomeNest = getHomeNest . envGrid <$> ask

data PlaceResult
  = HasPlaced
  | HitWall
  | HasOccupant Ix1
  deriving Show

placeAntInCell ::
     (MonadIO m, PrimMonad m) => MArray (PrimState m) P Ix2 Ix1 -> Ix2 -> Ix1 -> m PlaceResult
placeAntInCell grid cellIx antIx =
  casIntArray grid cellIx (coerce emptyCell) antIx >>= \case
    Nothing ->
      throwString $ "Should not attempt to write into out of bounds cell: " <> show cellIx
    Just prevIx
      | Cell prevIx == emptyCell -> pure HasPlaced
      | Cell prevIx == wallCell -> pure HitWall
      | otherwise -> pure $ HasOccupant prevIx

-- | Assumption is the is that the ant is at the specified location otherwise error.
removeAntFromCell ::
     (MonadIO m, PrimMonad m) => MArray (PrimState m) P Ix2 Ix1 -> Ix2 -> Ix1 -> m ()
removeAntFromCell grid cellIx antIx =
  casIntArray grid cellIx antIx (coerce emptyCell) >>= \case
    Nothing ->
      throwString $ "Should not attempt to write into out of bounds cell: " <> show cellIx
    Just prevIx
      | prevIx == antIx -> pure ()
      | otherwise ->
        throwString $
        "There was no ant at specified location: " <> show cellIx <> " discovered: " <>
        show prevIx <>
        " instead"

getNestIndices' :: Nest -> Array U Ix1 Ix2
getNestIndices' nest =
  computeAs U $ flatten (nestNorthWest nest + 1 ... nestNorthWest nest + unSz (nestSize nest) - 1)

getNestIndices :: Nest -> Array D Ix2 Ix2
getNestIndices nest = nestNorthWest nest + 1 ... nestNorthWest nest + unSz (nestSize nest) - 1


data GridScale
  = GridScale2x2
  | GridScale3x4
  | GridScale5x6
  deriving (Eq, Show)

data GridSpec = GridSpec
  { gridSpecSize     :: !Sz2
  , gridSpecScale    :: !GridScale
  , gridSpecNests    :: !(Array B Ix1 Nest)
  , gridSpecMaxSteps :: !Int
  } deriving Show

data Config = Config
  { configSeed      :: !(Word64, Word64)
  , configGridSpec  :: !GridSpec
  , configWorkers   :: !Sz1
  , configBrood     :: !Sz1
  , configSteps     :: !(Maybe Int)
  -- ^ Number of steps to terminate simulation after if a new nest doesn't get found
  , configVerbose   :: !Bool
  , configConstants :: !Constants
  , configMaxSteps  :: !Int
  -- ^ What is the maximum number of steps to run the model for
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


newColony :: IORef SMGen -> Grid -> Sz1 -> Sz1 -> IO Colony
newColony genRef grid@Grid {gridMap} workerCount broodCount = do
  colonyGrid <- loadArrayS $ A.map clearNestsIndices gridMap
  antTypes <-
    concatM
      1
      [ A.replicate Seq workerCount Worker
      , A.replicate Seq broodCount Brood
      , A.replicate Seq queenCount Queen :: Array D Ix1 AntType
      ]
  antDiscovered <- newIORef (Set.singleton 0)
  let homeIndices = getNestIndices (getHomeNest grid)
      homeIndicesCorner =
        extract' 0 (Sz (liftIndex (`div` 3) (unSz (A.size homeIndices)))) homeIndices
      -- if home is smaller than number of ants, this will loop forever
      getAvailableSpotFor antIx region = do
        mix <- runRIO genRef $ randomElement region
        ix <- maybe (error "Region is empty") pure mix
        placeAntInCell colonyGrid ix antIx >>= \case
          HasPlaced -> pure ix
          _ -> getAvailableSpotFor antIx region
      initAnt antIx antType = do
        let region =
              if antType == Worker
                then homeIndices
                else homeIndicesCorner
        locIx <- getAvailableSpotFor antIx region
        antLocation <- newIORef locIx
        antState <- newIORef $ Idle (Passive (getHomeNest grid) 0 (Walk 0 0))
        pure Ant {..}
  colonyAnts <- itraversePrim initAnt $ computeAs B antTypes
  colonyWorkers <-
    extractM 0 workerCount colonyAnts -- $ setComp (ParN (fromIntegral (unSz workerCount))) colonyAnts
  -- Initialize 30% of workers to scouts, the rest will passively hang out at a broken home
  scouts <- extractM 0 (Sz (unSz workerCount `div` 3)) colonyWorkers
  A.forM_ scouts $ \scout -> writeIORef (antState scout) (Idle (HangingAtHome 0 (Walk 0 0)))
  pure Colony {..}
  where
    queenCount = 1
    clearNestsIndices (Cell ix) =
      if ix >= 0
        then coerce emptyCell
        else ix



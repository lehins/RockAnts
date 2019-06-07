{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module RockAnts.Run where

import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import RIO
import RIO.Directory
import Text.Printf
import System.Random.SplitMix (seedSMGen')

import RockAnts.Grid
import RockAnts.Types
import RockAnts.Model

newEnv :: Config -> LogFunc -> IO Env
newEnv Config {..} envLogFunc = do
  let envGrid = makeGrid configGridSpec
      envConstants = configConstants
  envGen <- newIORef (seedSMGen' configSeed)
  envColony <- newColony envGen envGrid configWorkers configBrood
  pure Env {..}


testGridSpec :: GridSpec
testGridSpec =
  GridSpec
    (Sz2 200 300)
    GridScale3x4
    [ Nest
        { nestIx = 0
        , nestQuality = 0
        , nestNorthWest = 50 :. 20
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 94 :. 90
        , nestEntranceEnd = 97 :. 90
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.9
        , nestNorthWest = 50 :. 210
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 94 :. 210
        , nestEntranceEnd = 97 :. 210
        }
    ]
    1000

testGridSpecEq :: GridSpec
testGridSpecEq =
  GridSpec
    (Sz2 300 300)
    GridScale3x4
    [ Nest
        { nestIx = 0
        , nestQuality = 0
        , nestNorthWest = 100 :. 20
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 148 :. 90
        , nestEntranceEnd = 152 :. 90
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.7
        , nestNorthWest = 30 :. 210
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 78 :. 210
        , nestEntranceEnd = 82 :. 210
        }
    , Nest
        { nestIx = 2
        , nestQuality = 0.9
        , nestNorthWest = 170 :. 210
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 218 :. 210
        , nestEntranceEnd = 222 :. 210
        }
    ]
    2000

testGridSpec2 :: GridSpec
testGridSpec2 =
  GridSpec
    (Sz2 300 300)
    GridScale3x4
    [ Nest
        { nestIx = 0
        , nestQuality = 0
        , nestNorthWest = 50 :. 20
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 94 :. 90
        , nestEntranceEnd = 97 :. 90
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.7
        , nestNorthWest = 50 :. 210
        , nestSize = Sz (60 :. 70)
        , nestEntranceStart = 74 :. 210
        , nestEntranceEnd = 77 :. 210
        }
    , Nest
        { nestIx = 2
        , nestQuality = 0.9
        , nestNorthWest = 170 :. 210
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 224 :. 210
        , nestEntranceEnd = 227 :. 210
        }
    ]
    2000

testGridSpec3 :: GridSpec
testGridSpec3 =
  GridSpec
    (Sz2 300 400)
    GridScale3x4
    [ Nest
        { nestIx = 0
        , nestQuality = 0
        , nestNorthWest = 50 :. 20
        , nestSize = Sz (100 :. 70)
        , nestEntranceStart = 94 :. 90
        , nestEntranceEnd = 97 :. 90
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.6
        , nestNorthWest = 20 :. 300
        , nestSize = Sz (60 :. 70)
        , nestEntranceStart = 44 :. 300
        , nestEntranceEnd = 47 :. 300
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.7
        , nestNorthWest = 100 :. 300
        , nestSize = Sz (70 :. 70)
        , nestEntranceStart = 124 :. 300
        , nestEntranceEnd = 127 :. 300
        }
    , Nest
        { nestIx = 2
        , nestQuality = 0.8
        , nestNorthWest = 200 :. 300
        , nestSize = Sz (80 :. 70)
        , nestEntranceStart = 244 :. 300
        , nestEntranceEnd = 247 :. 300
        }
    ]
    4000

testConfig :: Config
testConfig =
  Config
    { configSeed = (7189523021148931526, 5721129601325187147)
    , configGridSpec = testGridSpecEq
    , configWorkers = 150
    , configBrood = 20
    , configSteps = Just 3
    , configVerbose = True
    , configConstants =
        Constants
          { constPpm = 0.1
          , constWms = 50
          , constMav = pi / 12
          , constAts = 30
          , constPap = 0
          , constMaxSteps = 100
          }
    , configMaxSteps = 20000
    }

runTest :: IO ()
runTest = do
  logOpts <- logOptionsHandle stdout True
  withLogFunc (setLogMinLevel LevelInfo logOpts) $ \logFunc -> do
    env <- newEnv config logFunc
    runRIO env $ do
      void $ tryIO $ removeDirectoryRecursive "output"
      createDirectoryIfMissing True "output"
      logInfo "Starting the simulation"
      runModel 0
      iGrid <- freeze Seq (colonyGrid (envColony env))
      logDebug $ displayShow iGrid
  where
    config = testConfig
    maxNumSteps = configMaxSteps config
    maxNumLen = length (show maxNumSteps)
    runModel i
      | i < maxNumSteps = do
        img <- makeColonyImage
        let path = printf ("output/step_%0" <> show maxNumLen <> "d.png") i
        liftIO $ writeImage path img -- $ computeAs S $ zoomWithGrid 128 (Stride 12) img
        colonyStep
        logSticky $ "Step: " <> display i <> "/" <> display maxNumSteps
        runModel (i + 1)
      | otherwise = logStickyDone $ "Done: " <> display i <> " steps"

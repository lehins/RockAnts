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

testConfig :: Config
testConfig =
  Config
    { configSeed = (7189523021148931526, 5721129601325187147)
    , configGridSpec = testGridSpec
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
    , configMaxSteps = 10000
    }

runTest :: IO ()
runTest = do
  logOpts <- logOptionsHandle stdout True
  withLogFunc (setLogMinLevel LevelInfo logOpts) $ \logFunc -> do
    env <- newEnv config logFunc
    runRIO env $ do
      let workers = colonyWorkers (envColony env)
      -- Initialize 30% of workers to scouts, the rest will passively hang out at a broken home
      scouts <- extractM 0 (Sz (unSz (size workers) `div` 3)) workers
      A.forM_ scouts $ \worker ->
        withAntTask_ worker (\_ -> pure $ HangingAtHome 0 (Walk 0 0))
      void $ tryIO $ removeDirectoryRecursive "output"
      createDirectoryIfMissing True "output"
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

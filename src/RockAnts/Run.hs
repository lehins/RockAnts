{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module RockAnts.Run where

import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Massiv.Array.Manifest.Vector as A
import RIO
import RIO.Directory
import Text.Printf
import System.Random.MWC

import RockAnts.Grid
import RockAnts.Types
import RockAnts.Model

newEnv :: Config -> LogFunc -> IO Env
newEnv Config {..} envLogFunc = do
  envGen <- restore configSeed
  let envGrid = makeGrid configGridSpec
      envConstants = configConstants
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
    { configSeed = seed
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

seed :: Seed
seed = toSeed $ fromMaybe (error "Impossible: seed conversion failed") $ A.castToVector (A.fromList Seq [3251359472,3403677325,1891750291,2843478301,3370756575,1467479369,462510664,1942071501,1041035751,2231891862,1160634009,1994868869,2177222714,1780170021,204838484,2860215498,2460947764,1864814981,1310269765,3132487852,1156898727,2892644458,2717077032,3029975425,3115822623,3445849001,1703241313,3656880682,1237927666,2872201400,4015033166,3641580347,1858056135,2684574518,2139707691,3373051500,4103759042,1051670615,1081745985,3436554238,3176952164,1269609336,1433818241,1730622026,692373262,3186389929,1029547216,1306210638,1152541850,1427805818,51606081,172756697,4097832011,46045140,3744848760,4271978338,2572578633,3382364785,2023708738,1474525082,1978296732,1383142210,91759973,2790288618,875185183,832154709,4253005016,1004545744,455204320,4173678201,3565606108,1096473374,179201359,1566421512,686882886,2062066553,1828991035,61325552,1042202898,1187254529,324404594,1537891613,3162780738,1358527556,44044098,2355828979,2417044405,3904171679,850373163,1071684493,3246409848,1583383342,1186883020,3867453263,752225917,3575643628,3834620280,1980012187,3245306639,4269732263,109923217,518229747,2886469813,3197068925,1791554757,1104847301,4001871496,3926618630,721993898,3097819825,3033768592,1037871429,604226706,945579171,673780767,3941820188,1248791883,1837690599,666964556,918453228,903242257,3125999766,2911209665,2845789135,703335210,2961274252,4281967727,305716798,3977829513,3790935562,1831176645,3372788761,2006948787,1906061264,3112474146,4265070272,3897976385,88219378,1024392943,3508486547,192971558,2076427618,4137306739,626615723,3820573504,512181313,3601434162,2640542923,677109905,645559182,2683283622,538467635,2367284717,1431636344,571051414,2809823366,3076174263,385948693,56089485,2118485656,3899198771,109329550,1712626849,114484565,3701693054,2186341183,1669324731,3609555354,1729929485,1257890898,840951607,3051231655,2970937548,1234792581,1866383771,3584298447,3479069997,1855622300,2686087959,26280793,2344727780,2895289777,3792833193,1981518069,331236242,1206131543,303279307,3177305856,653677926,2834945692,967251320,3403149918,2476145905,1654445268,3176349991,2762154814,3176000557,524228387,3785137553,569867379,3857078290,3667396380,1841593625,327402191,3102668985,1171869341,3037931416,2041229448,2559090454,203101173,3821801331,1007544772,312880729,3377136876,3081593903,455710128,3015325201,1072454238,4192242649,3607259544,3063668588,560592234,3498791086,2270058209,2263972527,3348244794,3856590131,2325532407,3267630278,3382725223,3380315002,3871899893,1506603637,3400096773,1097571178,2748967189,3094033163,3403373630,2979469657,2382601670,2851428589,4192346685,986070350,2945835777,2754536943,3727786584,2970832773,2170905383,3833751094,213447056,2732728819,365848539,1179542064,2514699479,189264364,3982685790,255,362436] :: Array P Ix1 Word32)

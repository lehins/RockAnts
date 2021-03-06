{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module RockAnts.Grid where

import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Graphics.ColorSpace
import RIO as P
import RockAnts.Types


-------------------
-- Grid creation --
-------------------


makeGrid :: GridSpec -> Grid
makeGrid gridSpec@GridSpec {gridSpecNests, gridSpecScale, gridSpecMaxSteps} =
  Grid
    { gridNests = gridSpecNests
    , gridMap = compute $ makeGridMap gridSpec
    , gridImage = compute $ makeGridImage gridSpec cellDrawer
    , gridCellDrawer = cellDrawer
    , gridSearchSteps = gridSpecMaxSteps
    }
  where
    cellDrawer = getCellDrawer gridSpecScale

fillWalls :: Monad m => (Ix2 -> m b) -> Ix2 -> Ix2 -> m ()
fillWalls writeWallCell nw@(north :. west) se@(south :. east) =
  let ne = north :. east
      sw = south :. west
   in P.mapM_ (A.mapM_ writeWallCell) [x ... y | (x, y) <- [(nw, ne), (ne, se), (nw, sw), (sw, se)]]


makeGridMap :: GridSpec -> Array DL Ix2 Cell
makeGridMap GridSpec {gridSpecSize, gridSpecNests} =
  makeLoadArrayS gridSpecSize emptyCell $ \writeCell -> do
    fillWalls (`writeCell` wallCell) 0 (unSz gridSpecSize - 1)
    A.iforM_ gridSpecNests $ \ix nest@Nest {nestNorthWest = nw, nestSize = Sz nestSz} -> do
      let se = nw + nestSz - 1
      fillWalls (`writeCell` wallCell) nw se
      A.mapM_ (`writeCell` emptyCell) (nestEntranceRange nest)
      A.mapM_ (`writeCell` Cell ix) (nestIndices nest)

-- >>> :i Image

makeGridImage :: GridSpec -> CellDrawer -> Image DL RGB Word8
makeGridImage gridSpec@GridSpec {gridSpecSize} cellDrawer =
  makeLoadArrayS imageSize whitePx $ \writePixel -> do
    let drawWallCell = drawCell cellDrawer (`writePixel` blackPx)
        drawEmptyCell = drawCell cellDrawer (`writePixel` whitePx)
    fillWalls drawWallCell 0 (unSz gridSpecSize - 1)
    fillNests gridSpec drawEmptyCell drawWallCell (\_ _ -> drawEmptyCell)
  where
    imageSize = csCellSize cellDrawer * gridSpecSize - Sz (csCellOffset cellDrawer)
    whitePx = maxBound :: Pixel RGB Word8
    blackPx = minBound :: Pixel RGB Word8

fillNests ::
     Monad m => GridSpec -> (Ix2 -> m ()) -> (Ix2 -> m ()) -> (Cell -> Nest -> Ix2 -> m ()) -> m ()
fillNests GridSpec {gridSpecNests} fillEmpty drawWallCell fillNest =
  A.iforM_ gridSpecNests $ \ix nest@Nest {nestNorthWest = nw, nestSize = Sz nestSz} -> do
    let se = nw + nestSz - 1
    fillWalls drawWallCell nw se
    A.mapM_ fillEmpty (nestEntranceRange nest)
    A.mapM_ (fillNest (Cell ix) nest) (nestIndices nest)



-- | Fill in a 2x2 block
--
drawCell2x2 :: Monad m => (Ix2 -> m a) -> Ix2 -> m ()
drawCell2x2 w (i0 :. j0) =
  w (i0 :. j0) >> w (i0 :. j1) >>
  w (i1 :. j0) >> w (i1 :. j1) >>
  pure ()
  where
    !i1 = i0 + 1
    !j1 = j0 + 1
{-# INLINE drawCell2x2 #-}

-- | Fill in a 4x4 cross like shape
drawCell4x4 :: Monad m => (Ix2 -> m a) -> Ix2 -> m ()
drawCell4x4 w (i0 :. j0) =
                  w (i0 :. j1) >> w (i0 :. j2) >>
  w (i1 :. j0) >> w (i1 :. j1) >> w (i1 :. j2) >> w (i1 :. j3) >>
  w (i2 :. j0) >> w (i2 :. j1) >> w (i2 :. j2) >> w (i2 :. j3) >>
                  w (i3 :. j1) >> w (i3 :. j2) >>
  pure ()
  where
    !i1 = i0 + 1
    !i2 = i0 + 2
    !i3 = i0 + 3
    !j1 = j0 + 1
    !j2 = j0 + 2
    !j3 = j0 + 3
{-# INLINE drawCell4x4 #-}



-- | Fill in a 7x6 cross like shape
drawCell7x6 :: Monad m => (Ix2 -> m a) -> Ix2 -> m ()
drawCell7x6 w (i0 :. j0) =
                                  w (i0 :. j2) >> w (i0 :. j3) >>
                  w (i1 :. j1) >> w (i1 :. j2) >> w (i1 :. j3) >> w (i1 :. j4) >>
  w (i2 :. j0) >> w (i2 :. j1) >> w (i2 :. j2) >> w (i2 :. j3) >> w (i2 :. j4) >> w (i2 :. j5) >>
  w (i3 :. j0) >> w (i3 :. j1) >> w (i3 :. j2) >> w (i3 :. j3) >> w (i3 :. j4) >> w (i3 :. j5) >>
  w (i4 :. j0) >> w (i4 :. j1) >> w (i4 :. j2) >> w (i4 :. j3) >> w (i4 :. j4) >> w (i4 :. j5) >>
                  w (i5 :. j1) >> w (i5 :. j2) >> w (i5 :. j3) >> w (i5 :. j4) >>
                                  w (i6 :. j2) >> w (i6 :. j3) >>
  pure ()
  where
    !i1 = i0 + 1
    !i2 = i0 + 2
    !i3 = i0 + 3
    !i4 = i0 + 4
    !i5 = i0 + 5
    !i6 = i0 + 6
    !j1 = j0 + 1
    !j2 = j0 + 2
    !j3 = j0 + 3
    !j4 = j0 + 4
    !j5 = j0 + 5
{-# INLINE drawCell7x6 #-}


drawCell :: Monad m => CellDrawer -> (Ix2 -> m a) -> Ix2 -> m ()
drawCell CellScaler {csCellOffset = csCellOffset@(vOffset :. _), ..} writePixel ix@(i :. _)
  | odd i = csDrawCell writePixel (ix * unSz csCellSize - (vOffset :. 0))
  | otherwise = csDrawCell writePixel (ix * unSz csCellSize - csCellOffset)


getCellDrawer :: GridScale -> CellDrawer
getCellDrawer gridScale =
  case gridScale of
    GridScale2x2 -> CellScaler (Sz2 2 2) (0 :. 1) drawCell2x2
    GridScale3x4 -> CellScaler (Sz2 3 4) (1 :. 2) drawCell4x4
    GridScale5x6 -> CellScaler (Sz2 5 6) (2 :. 3) drawCell7x6


getAntColor :: MonadIO m => Ant -> m (Pixel RGB Word8)
getAntColor Ant {antType, antState} = do
  state <- readIORef antState
  pure $
    case antType of
      Queen -> PixelRGB 255 000 000 -- Red
      Brood -> PixelRGB 100 100 100 -- Gray
      Worker ->
        case stateTask state of
          Passive {}         -> PixelRGB 000 000 255 -- Blue
          Searching {}       -> PixelRGB 153 000 000 -- Dark Red
          HangingAtHome {}   -> PixelRGB 000 153 000 -- Green
          Assessing {}       -> PixelRGB 102 102 000 -- Dark Yellow
          Recruiting {}      -> PixelRGB 000 153 153 -- Dark Cyan
          TandemLeading {}   -> PixelRGB 153 000 153 -- Dark Purple
          TandemFollowing {} -> PixelRGB 255 051 255 -- Light Purple
          Transporting {}    -> PixelRGB 255 128 000 -- Orange
          Transported {}     -> PixelRGB 000 102 000 -- Dark Green


makeColonyImage :: RIO Env (Image S RGB Word8)
makeColonyImage = do
  env <- ask
  let Grid{gridImage, gridCellDrawer} = envGrid env
      Colony {colonyAnts} = envColony env
  withMArray gridImage $ \_ _ image ->
    A.forIO_ (setComp Par colonyAnts) $ \ant@Ant {antLocation} -> do
      ix <- readIORef antLocation
      color <- getAntColor ant
      drawCell gridCellDrawer (\i -> write' image i color) ix



-----------------
-- Dev helpers --
-----------------


-- | Scale the array, create an array with a grid.
zoomWithGridD :: Manifest r Ix2 e => e -> Int -> Array r Ix2 e -> Array D Ix2 e
zoomWithGridD gridVal zoomFactor arr = A.makeArray (getComp arr) sz' getNewElt
  where
    k = zoomFactor + 1
    Sz (m :. n) = size arr
    sz' = Sz (1 + m * k :. 1 + n * k)
    getNewElt (i :. j) =
      if i `mod` k == 0 || j `mod` k == 0
        then gridVal
        else arr ! ((i - 1) `div` k :. (j - 1) `div` k)

-- | Scale the array, create an array with a grid.
zoomWithGridDL :: Source r Ix2 e => e -> Int -> Array r Ix2 e -> Array DL Ix2 e
zoomWithGridDL gridVal zoomFactor arr =
  makeLoadArrayS newSz gridVal $ \writeElement ->
    A.iforM_ arr $ \ix e -> do
      let (i :. j) = ix * (k :. k)
      A.mapM_ (`writeElement` e) $ range Seq (i + 1 :. j + 1) (i + k :. j + k)
  where
    k = zoomFactor + 1
    newSz = 1 + size arr * fromIntegral k
{-# INLINE zoomWithGridDL #-}


sampleGridSpec :: GridSpec
sampleGridSpec =
  GridSpec
    (Sz2 20 30)
    GridScale5x6
    [ Nest
        { nestIx = 0
        , nestQuality = 0
        , nestNorthWest = 5 :. 2
        , nestSize = Sz (10 :. 7)
        , nestEntranceStart = 8 :. 8
        , nestEntranceEnd = 10 :. 8
        }
    , Nest
        { nestIx = 1
        , nestQuality = 0.9
        , nestNorthWest = 5 :. 20
        , nestSize = Sz (10 :. 7)
        , nestEntranceStart = 9 :. 20
        , nestEntranceEnd = 11 :. 20
        }
    ]
    1000

displayGridImage :: Grid -> IO ()
displayGridImage =
  displayImageUsing defaultViewer True . computeAs S . zoomWithGrid 128 (Stride 6) . gridImage


sampleGrid :: Grid
sampleGrid = makeGrid sampleGridSpec


-- >>> displayGridImage sampleGrid

-----------------------
-- Direction helpers --
-----------------------

to2piRange :: Double -> Double
to2piRange !phi
  | phi < 0 = to2piRange (phi + 2 * pi)
  | phi >= 2 * pi = to2piRange (phi - 2 * pi)
  | otherwise = phi


destinationToWalk :: Ix2 -> Ix2 -> Maybe Walk
destinationToWalk ix0@(i0 :. j0) ix1@(i1 :. j1)
  | ix0 == ix1 = Nothing
  | otherwise =
    Just
      Walk
        { walkStepsLeft = 1 + ((abs (i1 - i0) + abs (j1 - j0)) `div` 2)
        , walkDirection = to2piRange $ adjust + atan ratio
        }
  where
    !i' = fromIntegral (i1 - i0)
    !j' = fromIntegral (j1 - j0)
    !adjust =
      if i' < 0
        then pi / 2
        else -pi / 2
    !ratio
      | even i0 && odd i1 = (j' * sqrt 3 + sqrt 3 / 2) / (i' * 3 / 2)
      | odd i0 && even i1 = (j' * sqrt 3 - sqrt 3 / 2) / (i' * 3 / 2)
      | otherwise = j' * sqrt 3 / (i' * (3 / 2))

cellInDirection :: Ix2 -> Double -> Ix2
cellInDirection ix@(i :. j) phi'
  | phi < -pi / 6 = cellInDirection ix $ to2piRange phi'
  | phi < pi / 6 = i :. j + 1
  | odd i = oddCell
  | otherwise = evenCell
  where
    phi = phi' - pi / 6
    oddCell
      | phi < 3 * pi / 6 = i - 1 :. j + 1
      | phi < 5 * pi / 6 = i - 1 :. j
      | phi < 7 * pi / 6 = i :. j - 1
      | phi < 9 * pi / 6 = i + 1 :. j
      | phi < 11 * pi / 6 = i + 1 :. j + 1
      | otherwise = cellInDirection ix $ to2piRange phi'
    evenCell
      | phi < 3 * pi / 6 = i - 1 :. j
      | phi < 5 * pi / 6 = i - 1 :. j - 1
      | phi < 7 * pi / 6 = i :. j - 1
      | phi < 9 * pi / 6 = i + 1 :. j - 1
      | phi < 11 * pi / 6 = i + 1 :. j
      | otherwise = cellInDirection ix $ to2piRange phi'

cellNeighbors :: Ix2 -> [Ix2]
cellNeighbors (i :. j) =
  [i - 1 :. j, i + 1 :. j, i :. j - 1, i :. j + 1] ++
  if odd i
    then [i - 1 :. j + 1, i + 1 :. j + 1]
    else [i - 1 :. j - 1, i + 1 :. j - 1]


emptyGridSpec :: GridSpec
emptyGridSpec = GridSpec (Sz2 200 300) GridScale3x4 A.empty 1000

emptyGrid :: Grid
emptyGrid = makeGrid emptyGridSpec

-- | Direction gradient, as a sanity check.
gradient :: IO (Image S RGB Word8)
gradient =
  withMArray (gridImage emptyGrid) $ \_ _ image ->
    A.forM_ (1 ..: unSz (size (gridMap emptyGrid)) - 1) $ \ix ->
      let color =
            case destinationToWalk (100 :. 150) ix of
              Nothing -> PixelRGB 255 0 0
              Just Walk {..} ->
                PixelRGB 0 (round (walkDirection * 255 / (2 * pi))) (fromIntegral walkStepsLeft)
       in drawCell (gridCellDrawer emptyGrid) (\i -> write' image i color) ix

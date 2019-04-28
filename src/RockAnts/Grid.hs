{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module RockAnts.Grid where

import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import Data.Massiv.Array.Unsafe as A
import Graphics.ColorSpace
import RIO as P
import RockAnts.Types


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


-- -- | Fill in a 7x6 cross like shape
-- drawCell7x6 :: Monad m => (Ix2 -> m a) -> Ix2 -> m ()
-- drawCell7x6 w (i0 :. j0) =
--                                   w (i0 :. j2) >> w (i0 :. j3) >>
--                   w (i1 :. j1) >> w (i1 :. j2) >> w (i1 :. j3) >> w (i1 :. j4) >>
--   w (i2 :. j0) >> w (i2 :. j1) >> w (i2 :. j2) >> w (i2 :. j3) >> w (i2 :. j4) >> w (i2 :. j5) >>
--   w (i3 :. j0) >> w (i3 :. j1) >> w (i3 :. j2) >> w (i3 :. j3) >> w (i3 :. j4) >> w (i3 :. j5) >>
--   w (i4 :. j0) >> w (i4 :. j1) >> w (i4 :. j2) >> w (i4 :. j3) >> w (i4 :. j4) >> w (i4 :. j5) >>
--                   w (i5 :. j1) >> w (i5 :. j2) >> w (i5 :. j3) >> w (i5 :. j4) >>
--                                   w (i6 :. j2) >> w (i6 :. j3) >>
--   pure ()
--   where
--     !i1 = i0 + 1
--     !i2 = i0 + 2
--     !i3 = i0 + 3
--     !i4 = i0 + 4
--     !i5 = i0 + 5
--     !i6 = i0 + 6
--     !j1 = j0 + 1
--     !j2 = j0 + 2
--     !j3 = j0 + 3
--     !j4 = j0 + 4
--     !j5 = j0 + 5
-- {-# INLINE drawCell7x6 #-}


-- -- | Fill in a 6x7 cross like shape
-- drawCell6x7 :: Monad m => (Ix2 -> m a) -> Ix2 -> m ()
-- drawCell6x7 w (i0 :. j0) =
--                               w (i0:.j2) >> w (i0:.j3) >> w (i0:.j4) >>
--                 w (i1:.j1) >> w (i1:.j2) >> w (i1:.j3) >> w (i1:.j4) >> w (i1:.j5) >>
--   w (i2:.j0) >> w (i2:.j1) >> w (i2:.j2) >> w (i2:.j3) >> w (i2:.j4) >> w (i2:.j5) >> w (i2:.j6) >>
--   w (i3:.j0) >> w (i3:.j1) >> w (i3:.j2) >> w (i3:.j3) >> w (i3:.j4) >> w (i3:.j5) >> w (i3:.j6) >>
--                 w (i4:.j1) >> w (i4:.j2) >> w (i4:.j3) >> w (i4:.j4) >> w (i4:.j5) >>
--                               w (i5:.j2) >> w (i5:.j3) >> w (i5:.j4) >>
--   pure ()
--   where
--     !i1 = i0 + 1
--     !i2 = i0 + 2
--     !i3 = i0 + 3
--     !i4 = i0 + 4
--     !i5 = i0 + 5
--     !j1 = j0 + 1
--     !j2 = j0 + 2
--     !j3 = j0 + 3
--     !j4 = j0 + 4
--     !j5 = j0 + 5
--     !j6 = j0 + 6
-- {-# INLINE drawCell6x7 #-}


data GridScale
  = GridScale2x2
  | GridScale3x4
  | GridScale5x6




makeGridMap :: GridSpec -> Array DL Ix2 Cell
makeGridMap GridSpec {gridSpecSize, gridSpecNests} =
  makeLoadArrayS gridSpecSize emptyCell $ \writeCell -> do
    let fillWalls nw@(north :. west) se@(south :. east) =
          let ne = north :. east
              sw = south :. west
           in P.mapM_
                (A.mapM_ (`writeCell` wallCell))
                [x ... y | (x, y) <- [(nw, ne), (ne, se), (nw, sw), (sw, se)]]
    fillWalls 0 (unSz gridSpecSize - 1)
    A.iforM_ gridSpecNests $ \ix Nest { nestNorthWest = nw
                                  , nestSize = Sz nestSz
                                  , nestEntranceStart = start
                                  , nestEntranceEnd = end
                                  } -> do
      let sw = nw + nestSz
      fillWalls nw sw
      A.mapM_ (`writeCell` emptyCell) (start ... end)
      A.forM_ (nw + 1 ... sw - 1) (`writeCell` Cell ix)


fillNests ::
     Monad m => GridSpec -> (Ix2 -> m ()) -> (Ix2 -> m ()) -> (Cell -> Nest -> Ix2 -> m ()) -> m ()
fillNests GridSpec {gridSpecSize, gridSpecNests} fillEmpty fillWall fillNest = do
  let fillWalls nw@(north :. west) se@(south :. east) =
        let ne = north :. east
            sw = south :. west
         in P.mapM_
              (A.mapM_ fillWall)
              [x ... y | (x, y) <- [(nw, ne), (ne, se), (nw, sw), (sw, se)]]
  fillWalls 0 (unSz gridSpecSize - 1)
  A.iforM_ gridSpecNests $ \ix nest@Nest { nestNorthWest = nw
                                         , nestSize = Sz nestSz
                                         , nestEntranceStart = start
                                         , nestEntranceEnd = end
                                         } -> do
    let sw = nw + nestSz
    fillWalls nw sw
    A.mapM_ fillEmpty (start ... end)
    A.forM_ (nw + 1 ... sw - 1) (fillNest (Cell ix) nest)

makeGridImage :: GridSpec -> GridScale -> Image DL RGB Word8
makeGridImage gridSpec@GridSpec {gridSpecSize} gridScale =
  makeLoadArrayS imageSize whitePx $ \writePixel -> do
    let writePixelOff e ix = writePixel (ix - csCellOffset cellDrawer) e
        drawWallCell = drawCell cellDrawer (writePixelOff blackPx)
        drawEmptyCell = drawCell cellDrawer (writePixelOff whitePx)
    fillNests gridSpec drawEmptyCell drawWallCell (\_ _ -> drawEmptyCell)
  where
    imageSize = csCellSize cellDrawer * gridSpecSize - Sz (csCellOffset cellDrawer)
    cellDrawer = getCellDrawer gridScale
    whitePx = maxBound :: Pixel RGB Word8
    blackPx = minBound :: Pixel RGB Word8


makeGrid :: GridSpec -> GridScale -> Grid
makeGrid gridSpec@GridSpec {gridSpecNests} gridScale =
  Grid
    { gridNests = gridSpecNests
    , gridMap = compute $ makeGridMap gridSpec
    , gridImage = compute $ makeGridImage gridSpec gridScale
    }


drawCell :: Monad m => CellDrawer -> (Ix2 -> m a) -> Ix2 -> m ()
drawCell CellScaler {..} writePixel ix@(i :. _)
  | odd i = csDrawCell writePixel (ix * unSz csCellSize + (0 :. 1) * csCellOffset)
  | otherwise = csDrawCell writePixel (ix * unSz csCellSize)

data Orientation
  = Horizontal
  | Vertical
  deriving (Eq, Show)

data CellDrawer = CellScaler
  { csCellSize   :: !Sz2
  , csCellOffset :: !Ix2
  , csDrawCell   :: forall m a. Monad m => (Ix2 -> m a) -> Ix2 -> m ()
  }

getCellDrawer :: GridScale -> CellDrawer
getCellDrawer gridScale =
  case gridScale of
    GridScale2x2 -> CellScaler (Sz2 2 2) (0 :. 1) drawCell2x2
    GridScale3x4 -> CellScaler (Sz2 3 4) (1 :. 2) drawCell4x4
    GridScale5x6 -> CellScaler (Sz2 5 6) (2 :. 3) drawCell7x6



data GridSpec = GridSpec
  { gridSpecSize  :: !Sz2
  , gridSpecNests :: !(Array B Ix1 Nest)
  } deriving Show



testNests :: GridSpec
testNests =
  GridSpec 20 $
  A.singleton
    Nest
      { nestScore = 0
      , nestNorthWest = 2 :. 2
      , nestSize = 10
      , nestEntranceStart = 4 :. 12
      , nestEntranceEnd = 6 :. 12
      }


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

-- -- | Scale the array, create an array with a grid.
-- zoomWithGridDL :: Source r Ix2 e => e -> Int -> Array r Ix2 e -> Array DL Ix2 e
-- zoomWithGridDL gridVal zoomFactor arr =
--   makeLoadArrayS newSz gridVal $ \writeElement -> do
--     A.iforM_ arr $ \ix e -> do
--       let (i :. j) = ix * (k :. k)
--       A.mapM_ (`writeElement` e) $ range Seq (i + 1 :. j + 1) (i + k :. j + k)
--   where
--     k = zoomFactor + 1
--     newSz = 1 + size arr * fromIntegral k
-- {-# INLINE zoomWithGridDL #-}

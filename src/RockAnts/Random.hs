{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module RockAnts.Random where

import Data.Massiv.Array as A
import RIO
import System.Random.SplitMix


class HasGen env where
  genG :: SimpleGetter env (IORef SMGen)

instance HasGen (IORef SMGen) where
  genG = id


randomElement ::
     (HasGen env, Source r ix e) => Array r ix e -> RIO env (Maybe e)
randomElement arr
  | A.isEmpty arr = pure Nothing
  | otherwise = do
      let sz = size arr
          k = totalElem sz
      i <- randomIntRange (0, k - 1)
      -- We need to fail loud if index is wrong, since that should never happen
      e <- evaluateM arr (fromLinearIndex sz i)
      pure $ Just e


randomBool :: HasGen env => RIO env Bool
randomBool = do
  genRef <- view genG
  even <$> atomicModifyIORef' genRef (swapTuple . nextInt)

randomIntRange :: HasGen env => (Int, Int) -> RIO env Int
randomIntRange rangeIncl = do
  genRef <- view genG
  atomicModifyIORef' genRef (nextIntRange rangeIncl)

randomDouble :: HasGen env => RIO env Double
randomDouble = do
  genRef <- view genG
  atomicModifyIORef' genRef (swapTuple . nextDouble)

randomDoubleRange :: HasGen env => (Double, Double) -> RIO env Double
randomDoubleRange (x, y)
  | x == y = pure x
  | otherwise = do
    genRef <- view genG
    rand <- atomicModifyIORef' genRef (swapTuple . nextDouble)
    pure (rand * (y - x) + x)

randomDoubleRangeInclusive :: HasGen env => (Double, Double) -> RIO env Double
randomDoubleRangeInclusive (x, y) = randomDoubleRange (x, y + 2 ** (-53))


-- | Generates an angle in radians in range @[0, 2*pi)@ as `Double`
randomDirection :: HasGen env => RIO env Double
randomDirection = (2*pi*) <$> randomDouble

swapTuple :: (b, a) -> (a, b)
swapTuple (a, b) = (b, a)

-- Adjusted copy from mwc-random
nextIntRange :: (Int, Int) -> SMGen -> (SMGen, Int)
nextIntRange (x1, x2) g
  | n == 0 = swapTuple $ nextInt g -- Abuse overflow in unsigned types
  | otherwise = go g
  where
    (i, j) -- Allow ranges where x2<x1
      | x1 < x2 = (x1, x2)
      | otherwise = (x2, x1)
    n = 1 + (fromIntegral j - fromIntegral i) :: Word
    buckets = maxBound `div` n
    maxN = buckets * n
    go gen =
      let (gen', x) = fromIntegral <$> swapTuple (nextInt gen)
      in if x < maxN
         then (gen', i + fromIntegral (x `div` buckets))
         else go gen'




-- | T. albipennis scouts show behavioural lateralization when exploring unknown
-- nest sites, showing a population-level bias to prefer left turns. One
-- possible reason for this is that its environment is partly maze-like and
-- consistently turning in one direction is a good way to search and exit mazes
-- without getting lost.
addDirectionDelta :: HasGen env => Double -> RIO env Double
addDirectionDelta direction = do
  delta <- randomDoubleRangeInclusive (-pi / 12, pi / 12 + pi / 216)
  pure (direction + delta)

randomTurn :: HasGen env => Double -> RIO env Double
randomTurn direction = do
  turnLeft <- randomBool
  pure $
    if turnLeft
      then direction + (pi / 3 + pi / 216) -- Rock ants are slightly left biased
      else direction - pi / 3

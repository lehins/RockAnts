{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module RockAnts.Random where

import Data.Massiv.Array as A
import RIO
import System.Random.MWC

class HasGen env where
  genG :: SimpleGetter env (Gen RealWorld)

instance HasGen (Gen RealWorld) where
  genG = id

randomElement ::
     (HasGen env, Resize r ix, Source r Ix1 e, Load r ix e) => Array r ix e -> RIO env (Maybe e)
randomElement arr
  | A.isEmpty arr = pure Nothing
  | otherwise = do
      let arr' = flatten arr
      ix <- uniformRange (0, unSz (size arr') - 1)
      pure $ evaluateM arr' ix

-- | Generates an angle inradians in range @[0, 2*pi)@ as `Double`
randomDirection :: HasGen env => RIO env Double
randomDirection = subtract (2 ** (-53)) <$> uniformRange (0, 2 * pi)

uniformExclusive :: HasGen env => (Double, Double) -> RIO env Double
uniformExclusive (x, y) = uniformRange (x, y - 2 ** (-53))

uniformRandom :: (Variate a, HasGen env) => RIO env a
uniformRandom = view genG >>= uniform

-- | For integral types range is inclusive, for floating point numbers range is: (a, b]
uniformRange :: (Variate a, HasGen env) => (a, a) -> RIO env a
uniformRange r = do
  gen <- view genG
  uniformR r gen

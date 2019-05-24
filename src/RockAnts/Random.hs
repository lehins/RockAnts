{-# LANGUAGE FlexibleContexts #-}
module RockAnts.Random where

import Data.Massiv.Array as A
import RIO
import System.Random.MWC

class HasGen env where
  genG :: SimpleGetter env (Gen (PrimState (RIO env)))

randomElement :: (HasGen env, Source r Ix1 e) => Array r Ix1 e -> RIO env (Maybe e)
randomElement arr
  | A.isEmpty arr = pure Nothing
  | otherwise = do
      gen <- view genG
      ix <- uniformR (0, unSz (size arr)) gen
      pure $ evaluateM arr ix

-- | Generates an angle inradians in range @[0, 2*pi)@ as `Double`
randomDirection :: HasGen env => RIO env Double
randomDirection = do
  gen <- view genG
  subtract (2**(-53)) <$> uniformR (0, 2 * pi) gen

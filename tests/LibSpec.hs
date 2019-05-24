{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module LibSpec (spec) where

import Common
import Data.Massiv.Array as A
import RockAnts.Grid
import RockAnts.Types

spec :: Spec
spec = do
  describe "properties" $ do
    it "toFromDestination" $
      property $ \(Positive i, Positive j) ->
        let ix = i :. j
            dixGen =
              elements $
              [0 :. -1, 0 :. 1, -1 :. 0, 1 :. 0] ++
              if odd i
                then [-1 :. 1, 1 :. 1]
                else [-1 :. -1, 1 :. -1]
         in forAll dixGen $ \dix ->
              let cell = ix + dix
                  {- adjust for Double imprecision -}
               in fmap
                    ((cellInDirection ix) . (+ 0.000001) . walkDirection)
                    (destinationToWalk ix cell) ===
                  Just cell

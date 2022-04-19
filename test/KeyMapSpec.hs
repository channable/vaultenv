{-# LANGUAGE ScopedTypeVariables #-}

module KeyMapSpec where

import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.HashMap.Internal.Strict as HM

import qualified KeyMap as KM

mapFirst :: (a->b) -> [(a,c)] -> [(b,c)]
mapFirst _ [] = []
mapFirst f ((x,y) : rest) = (f x, y) : mapFirst f rest

spec :: SpecWith ()
spec =
  describe "KeyMap" $ do
    it "lookupDefault matches the HashMap implementation" $
      property $ \((fallback :: Text), key, mapping) ->
        let keyMapValue = KM.lookupDefault fallback (KM.fromText key) (KM.fromList $ mapFirst KM.fromText mapping)
            hashMapValue = HM.lookupDefault fallback key (HM.fromList mapping)
        in keyMapValue `shouldBe` hashMapValue

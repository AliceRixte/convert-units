module Data.Type.IntSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Type.Int

spec :: Spec
spec = do
  describe "Data.Type.Int" $ do
    it "Singleton converts properly" $ do
      property $ \ (i :: Integer) ->
        withSomeSZZ i fromSZZ == i

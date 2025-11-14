{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.CoreProp
  ( module Data.Units.Core.ArithmeticProp
  , module Data.Units.Core.ConvertProp
  , module Data.Units.CoreProp
  )
  where

import Data.Units.Core.ArithmeticProp
import Data.Units.Core.ConvertProp

import Test.QuickCheck
import Test.Hspec

import Data.AEq

import Data.Units

sameDimSpec :: forall u a.
   (IsUnit u, Floating (u a), ShowUnit u
  , Show a, AEq a, Floating a, Eq a, Arbitrary a
  , FromTo' u u a
  , DimEq u u
  , IsUnit (NormalizeUnitL (u .^+ 2))
  , IsUnit (BaseUnitOf u)
  )
  => Spec
sameDimSpec = describe ("Unit " ++ showUnit @u) $ do
  floatingSpec @u @a
  addLeftSpec @u @u @a
  addRightSpec @u @u @a
  addSameSpec @u @u @a
  subLeftSpec @u @u @a
  subRightSpec @u @u @a
  subSameSpec @u @u @a
  mulSameSpec @u @u @a
  -- mulLeftSpec @u @u @a
  -- mulRightSpec @u @u @a
  divSameSpec @u @u @a
  expSpec @u @a
  expConvSpec @u @a


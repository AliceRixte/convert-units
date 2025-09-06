{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.BaseProp
  ( module Data.Units.Base.ArithmeticProp
  , module Data.Units.Base.ConvertProp
  , module Data.Units.BaseProp
  )
  where

import Data.Units.Base.ArithmeticProp
import Data.Units.Base.ConvertProp

import Test.QuickCheck
import Test.Hspec

import Data.Epsilon

import Data.Units

sameDimSpec :: forall u a.
   (IsUnit u, Floating (u a), ShowUnit u
  , Show a, Epsilon a, Floating a, Eq a, Arbitrary a
  , FromTo' u u a
  , DimEq u u
  , IsUnit (NormalizeUnitL (u .^+ 2))
  , IsUnit (NormalizeUnit u)
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


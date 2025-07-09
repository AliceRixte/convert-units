{-# LANGUAGE DeriveFunctor #-}

module Data.Units.Base.Dimension where

import Data.Kind
import GHC.TypeLits


type Dim = Type -> Type

type family DimId (d:: Dim) :: Nat
type family DimName (d :: Dim) :: Symbol


-- | The dimension of non dimensional quantities
--
newtype NoDim a = NoDim a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


type instance DimId NoDim = 0
type instance DimName NoDim = "nodim"

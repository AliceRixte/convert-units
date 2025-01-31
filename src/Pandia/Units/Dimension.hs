{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Pandia.Units.Dimension
  ( module Pandia.Units.Dimension
  ) where

import GHC.TypeLits

import Pandia.Units.Convertor


data Dimension l m t i th n j = Dimension l m t i th n j

type family MulDim (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                   (d':: Dimension Nat Nat Nat Nat Nat Nat Nat)
                      :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  MulDim ('Dimension l m t i th n j)
         ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

type family DivDim (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                   (d':: Dimension Nat Nat Nat Nat Nat Nat Nat)
                      :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  DivDim ('Dimension l m t i th n j)
         ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

type family PowDim (d :: Dimension Nat Nat Nat Nat Nat Nat Nat) (n :: Nat)
                      :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  PowDim ('Dimension l m t i th n j) n' =
    'Dimension (n * n) (m * n') (t * n') (i * n') (th * n') (n * n') (j * n')


-- | Are two dimensions equal?
data DimCheck =
    DimOK
  | DimError


-- | Used for better reading of error messages
newtype DiffDimIsNotZero d = DiffDimIsNotZero d



-- | This allows Haskell to print an error message showing the dimension
-- missmatch
--
-- For instance, when trying to convert kilo meter per second to meters, this shows the following error message:
--
-- @
-- >ghci> x = 4 :: (Kilo Meter -/- Second) Double
-- >ghci> asCheck x meter

-- ><interactive>:54:1: error: [GHC-18872]
-- >    • Couldn't match type ‘DimensionError
-- >                             ('Dimension 1 (0 GHC.TypeNats.- 1) 0 0 0 0 0)
-- >                             ('Dimension 1 0 0 0 0 0 0)
-- >                             ('DiffDimIsNotZero ('Dimension 0 (0 GHC.TypeNats.- 1) 0 0 0 0 0))’
-- >                     with ‘DimOK’
-- >        arising from a use of ‘asCheck’
-- >    • In the expression: asCheck x meter
-- >      In an equation for ‘it’: it = asCheck x meter
-- @
type family DimensionError
  (d1 :: Dimension Nat Nat Nat Nat Nat Nat Nat)
  (d2 :: Dimension Nat Nat Nat Nat Nat Nat Nat)
  (d1md2 :: DiffDimIsNotZero (Dimension Nat Nat Nat Nat Nat Nat Nat)) :: DimCheck where
  DimensionError _ _('DiffDimIsNotZero ('Dimension 0 0 0 0 0 0 0)) = 'DimOK
  DimensionError _ _ _ = 'DimError

type family DimEq (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                  (d' :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                        :: DimCheck where
  DimEq d d' = DimensionError d d' ('DiffDimIsNotZero (d `DivDim` d'))



class ToDimension (f :: Unit) where
  type ToDim f :: Dimension Nat Nat Nat Nat Nat Nat Nat

instance ToDimension NoUnit where
  type ToDim NoUnit = 'Dimension 0 0 0 0 0 0 0

instance (ToDimension (f :: Unit), ToDimension (g :: Unit))
  => ToDimension (f -*- g) where
  type ToDim (f -*- g) = ToDim f `MulDim` ToDim g

instance (ToDimension (f :: Unit), ToDimension (g :: Unit))
  => ToDimension (f -/- g) where
  type ToDim (f -/- g) = ToDim f `DivDim` ToDim g

instance (ToDimension (f :: Unit), KnownNat n)
  => ToDimension (f -^- n) where
  type ToDim (f -^- n) = ToDim f `PowDim` n


type SameDim f g = DimEq (ToDim f) (ToDim g) ~ 'DimOK

------------------------------------------------------------------------------

-- | A container for a quantity whose unit can change but whose dimension is a
-- length. You can only convert to another unit, and the conversion will be the
-- identity function.
newtype Length a = Length a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance ToDimension Length where
  type ToDim Length = 'Dimension 1 0 0 0 0 0 0

instance ConvertorClass Length (From a)

lengthTo :: Convertor Length (From a)
lengthTo = convertor
{-# INLINE lengthTo #-}



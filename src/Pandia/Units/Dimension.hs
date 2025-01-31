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


-- | Print both dimensions in case of error
data DimCheck d =
    DimOK
  | DimError d d


-- | This allows Haskell to print an error message showing the dimension
-- missmatch
--
-- For instance, when trying to convert kilo meter per second to meters, this shows the following error message:
--
-- @
-- ghci> x = 10 :: Second Int
-- ghci> x  `as` gram

-- <interactive>:86:4: error: [GHC-18872]
--     • Couldn't match type: DimError
--                              ('Dimension 0 0 1 0 0 0 0) ('Dimension 0 1 0 0 0 0 0)
--                      with: DimOK
-- @
type family DimensionError
  (d1 :: Dimension Nat Nat Nat Nat Nat Nat Nat)
  (d2 :: Dimension Nat Nat Nat Nat Nat Nat Nat)
  (d1md2 :: Dimension Nat Nat Nat Nat Nat Nat Nat)
  :: DimCheck (Dimension Nat Nat Nat Nat Nat Nat Nat) where

  DimensionError _ _ ('Dimension 0 0 0 0 0 0 0) = 'DimOK
  DimensionError d d' d1md2 = 'DimError d d'


type family DimEq (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                  (d' :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                  :: DimCheck (Dimension Nat Nat Nat Nat Nat Nat Nat) where
  DimEq d d' = DimensionError d d' (d `DivDim` d')



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
-- length.
--
newtype Length a = Length a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

instance ToDimension Length where
  type ToDim Length = 'Dimension 1 0 0 0 0 0 0



-- | Convert an unspecified length to some length unit. You can only convert to
-- that unit and not the other way around.
--
-- @
--  >>> (lengthTo ~> meter) 1
-- Meter 1
-- >>> (meter ~> lengthTo) 1
-- <interactive>:9:11: error: [GHC-83865]
--   • Couldn't match type: From a0
--                    with: To a
--     Expected: Convertor Length (To a)
--       Actual: Convertor Length (From a0)
-- @
--
-- lengthTo :: Convertor Length (From a)
-- lengthTo = convertor
-- {-# INLINE lengthTo #-}


-- | A container for a quantity whose unit can change but whose dimension is a
-- mass.
--
-- newtype Mass a = Mass a
--   deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
--            , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

-- instance ToDimension Mass where
--   type ToDim Mass = 'Dimension 0 1 0 0 0 0 0

-- instance ConvertorClass Mass (From a)

-- -- | Convert an unspecified mass to some mass unit. You can only convert to
-- -- that unit and not the other way around.
-- --
-- -- @
-- --  >>> (massTo ~> ton) 1 Meter 1
-- --  >>> (massTo ~> kilo gram) 1 Meter 1
-- -- 1.0
-- -- >>> (kilo gram ~> massTo) 1
-- -- <interactive>:9:11: error: [GHC-83865]
-- --   • Couldn't match type: From a0
-- --                    with: To a
-- --     Expected: Convertor Mass (To a)
-- --       Actual: Convertor Mass (From a0)
-- -- @
-- --
-- massTo :: Convertor Mass (From a)
-- massTo = convertor
-- {-# INLINE massTo #-}

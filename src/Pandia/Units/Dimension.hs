{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Pandia.Units.Dimension
  ( module Pandia.Units.Dimension
  ) where

import GHC.TypeLits

import Pandia.Units.Convertor
import Pandia.Units.Rel


data Dimension l m t i th n j = Dimension l m t i th n j

type DimNoUnit =
  'Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0)

type DimLength =
  'Dimension (Pos 1) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0)

type DimMass =
  'Dimension (Pos 0) (Pos 1) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0)

type DimTime =
  'Dimension (Pos 0) (Pos 0) (Pos 1) (Pos 0) (Pos 0) (Pos 0) (Pos 0)

type DimCurrent =
  'Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 1) (Pos 0) (Pos 0) (Pos 0)

type DimTemperature =
  'Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 1) (Pos 0) (Pos 0)

type DimAmount =
  'Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 1) (Pos 0)

type DimLuminousIntensity =
  'Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 1)

type family MulDim (d :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                   (d':: Dimension Rel Rel Rel Rel Rel Rel Rel)
                      :: Dimension Rel Rel Rel Rel Rel Rel Rel where
  MulDim ('Dimension l m t i th n j)
         ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l `SumRel` l') (m `SumRel` m') (t `SumRel` t') (i `SumRel` i') (th `SumRel` th') (n `SumRel` n') (j `SumRel` j')


type family NegateDim (d :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                      :: Dimension Rel Rel Rel Rel Rel Rel Rel where
  NegateDim ('Dimension l m t i th n j) =
    'Dimension (NegateRel l) (NegateRel m) (NegateRel t) (NegateRel i) (NegateRel th) (NegateRel n) (NegateRel j)

type family DivDim (d :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                   (d':: Dimension Rel Rel Rel Rel Rel Rel Rel)
                      :: Dimension Rel Rel Rel Rel Rel Rel Rel where
  DivDim ('Dimension l m t i th n j)
         ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l `SubRel` l') (m `SubRel` m') (t `SubRel` t') (i `SubRel` i') (th `SubRel` th') (n `SubRel` n') (j `SubRel` j')

type family PowDim (d :: Dimension Rel Rel Rel Rel Rel Rel Rel) (n :: Rel)
                      :: Dimension Rel Rel Rel Rel Rel Rel Rel where
  PowDim ('Dimension l m t i th n j) n' =
    'Dimension (n `MulRel` n) (m `MulRel` n') (t `MulRel` n') (i `MulRel` n') (th `MulRel` n') (n `MulRel` n') (j `MulRel` n')

type family NormalizeDim (d :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                      :: Dimension Rel Rel Rel Rel Rel Rel Rel where
  NormalizeDim ('Dimension l m t i th n j) =
    'Dimension (NormalizeRel l) (NormalizeRel m) (NormalizeRel t) (NormalizeRel i) (NormalizeRel th) (NormalizeRel n) (NormalizeRel j)


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
  (d1 :: Dimension Rel Rel Rel Rel Rel Rel Rel)
  (d2 :: Dimension Rel Rel Rel Rel Rel Rel Rel)
  (d1md2 :: Dimension Rel Rel Rel Rel Rel Rel Rel)
  :: DimCheck (Dimension Rel Rel Rel Rel Rel Rel Rel) where

  DimensionError _ _ ('Dimension (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0) (Pos 0)) = 'DimOK
  DimensionError d d' d1md2 = 'DimError d d'


type family DimEq (d :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                  (d' :: Dimension Rel Rel Rel Rel Rel Rel Rel)
                  :: DimCheck (Dimension Rel Rel Rel Rel Rel Rel Rel) where
  DimEq d d' = DimensionError d d' (d `DivDim` d')



class ToDimension (f :: Unit) where
  type ToDim f :: Dimension Rel Rel Rel Rel Rel Rel Rel

instance ToDimension NoUnit where
  type ToDim NoUnit = DimNoUnit

instance (ToDimension (f :: Unit), ToDimension (g :: Unit))
  => ToDimension (f -*- g) where
  type ToDim (f -*- g) = ToDim f `MulDim` ToDim g

instance (ToDimension (f :: Unit), ToDimension (g :: Unit))
  => ToDimension (f -/- g) where
  type ToDim (f -/- g) = ToDim f `DivDim` ToDim g

instance (ToDimension (f :: Unit))
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
  type ToDim Length =  DimLength



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

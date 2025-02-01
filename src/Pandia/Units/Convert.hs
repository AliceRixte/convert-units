{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Pandia.Units.Convert
  ( module Pandia.Units.Convert
  ) where

import Data.Coerce
import Data.Proxy

import Pandia.Units.Dimension
import Pandia.Units.Convertor
import Pandia.Units.SI
import Pandia.Units.Prefix
import Pandia.Units.Rel

-- type family NatToUnit (n :: Nat) :: Unit where
--   NatToUnit 0 = NoUnit
--   NatToUnit 1 = Unit
--   NatToUnit n  | n < 1 = Unit -*- NatToUnit (n - 1)



fromSI :: forall f a. Coercible a (f a)
  => Convertor f (To a) -> a -> f a
fromSI f a = coerce (f (Proxy :: Proxy f)) a


--  The following type families aim at implementing a normalization function that toSI can use (and it also would be useful in many other cases)

-- but really it does not work because we need Relative type level numbers instead of natural numbers

type family DimToSI (d :: Dimension Rel Rel Rel Rel Rel Rel Rel) :: Unit where
  DimToSI ('Dimension l m t i th n j) =
          (Meter     -^- l )
      -*- (Kilo Gram -^- m )
      -*- (Second    -^- t )
      -*- (Ampere    -^- i )
      -*- (Kelvin    -^- th)
      -*- (Mole      -^- n )
      -*- (Candela   -^- j )


type family PushNeg (f :: Unit) :: Unit where
  PushNeg (f -^- Neg n -*- g) = PushNeg f -^- Pos n


type family ElimDiv (f :: Unit) :: Unit where
    ElimDiv (f -*- g) = ElimDiv f -*- ElimDiv g
    ElimDiv (f -^- n) = ElimDiv f -^- n
    ElimDiv (f -/- f) = NoUnit
    ElimDiv (f -/- g) = ElimDiv f -*- ElimDiv g -^- Neg 1
    ElimDiv f = f

type family ElimPow (f :: Unit) :: Unit where
  ElimPow (f -/- g) = ElimPow f -/- ElimPow g
  ElimPow (f -*- g) = ElimPow f -*- ElimPow g
  ElimPow (f -^- Pos 0) = NoUnit
  ElimPow (f -^- Neg 0) = NoUnit
  ElimPow (f -^- Pos 1) = ElimPow f
  ElimPow ((f -^- n) -^- m) = ElimPow (f -^- (n `MulRel` m))
  ElimPow ((f -*- g) -^- n) = ElimPow (f -^- n -*- g -^- n)
  ElimPow f = f

type family ElimNoUnit (f :: Unit) :: Unit where
  ElimNoUnit (NoUnit -*- f) = ElimNoUnit f
  ElimNoUnit (f -*- NoUnit) = ElimNoUnit f
  ElimNoUnit (f -*- g) = ElimNoUnit f -*- ElimNoUnit g
  ElimNoUnit f = f

type family SimplifyUnit (f :: Unit) :: Unit where
  SimplifyUnit f = ElimNoUnit (ElimPow (ElimDiv f))



type family UnitToSI (f :: Unit) :: Unit where
  UnitToSI f = SimplifyUnit (DimToSI (ToDim f))


--  | This does NOT work ! Type inference will fail as soon as there is a negative exponent.
toSI :: forall f a. Coercible a ((UnitToSI f) a)
  => Convertor f (From a) -> a -> (UnitToSI f) a
toSI f = coerce (f (Proxy :: Proxy f))
{-# INLINE toSI #-}

asSI :: forall f a.
  (Coercible a (f a), Coercible a ((UnitToSI f) a), ConvertorClass f (From a))
  =>  f a -> (UnitToSI f) a
asSI fa = toSI (convertor :: Convertor f (From a)) (coerce fa :: a)
{-# INLINE asSI #-}

fromSI' :: forall f a. Convertor f (To a) -> a -> a
fromSI' f = coerce (f (Proxy :: Proxy f))
{-# INLINE fromSI' #-}

toSI' :: forall f a. Convertor f (From a) -> a -> a
toSI' f = coerce (f (Proxy :: Proxy f))
{-# INLINE toSI' #-}



fromToNoCheck :: forall f g a. (Coercible a (g a), Coercible a (f a))
   => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
fromToNoCheck f t a = coerce
   $ (coerce (t (Proxy :: Proxy g)) :: a -> a)
   $ (coerce (f (Proxy :: Proxy f)) :: a -> a)
   $ (coerce a :: a)
{-# INLINE fromToNoCheck #-}

fromTo :: forall f g a.
  (Coercible a (g a), Coercible a (f a), SameDim f g)
  => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
fromTo = fromToNoCheck
{-# INLINE fromTo #-}

fromToNoCheck' :: forall f g a.
  Convertor f (From a) -> Convertor g (To a) -> a -> a
fromToNoCheck' f t  = fromSI' t . toSI' f
{-# INLINE fromToNoCheck' #-}

fromTo' ::  forall f g a.
  Convertor f (From a) -> Convertor g (To a) -> a -> a
fromTo' = fromToNoCheck'
{-# INLINE fromTo' #-}
infix 2 `fromTo'`




(~>) :: forall f g a.
  (Coercible a (g a), Coercible a (f a), SameDim f g)
  => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall f g a.
  Convertor f (From a) -> Convertor g (To a) -> a -> a
(~~>) = fromToNoCheck'
{-# INLINE (~~>) #-}
infix 2 ~~>

asNoCheck :: forall f g a.
  (Coercible a (f a), Coercible a (g a), ConvertorClass f (From a))
  => f a -> Convertor g (To a) -> g a
asNoCheck fa g = fromToNoCheck (convertor :: Convertor f (From a)) g fa
{-# INLINE asNoCheck #-}


-- | Convert a quantity from one unit to the other, and checks if the unit's
-- dimensions are compatible
--
-- @
-- >>> x = 4 :: (Kilo Meter -/- Second) Double
-- >>> as x meter
-- <interactive>:54:1: error: [GHC-18872]
--     • Couldn't match type ‘DimensionError
--                              ('Dimension 1 (0 GHC.TypeNats.- 1) 0 0 0 0 0)
--                              ('Dimension 1 0 0 0 0 0 0)
--                              ('DiffDimIsNotZero ('Dimension 0 (0 GHC.TypeNats.- 1) 0 0 0 0 0))’
--                      with ‘DimOK’
--         arising from a use of ‘asCheck’
--     • In the expression: asCheck x meter
--       In an equation for ‘it’: it = asCheck x meter
-- @
as :: forall f g a.
  (Coercible a (f a), Coercible a (g a), ConvertorClass f (From a), SameDim f g)
  => f a -> Convertor g (To a) -> g a
as = asNoCheck
{-# INLINE as #-}
infix 2 `as`


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



fromSI :: forall u cd p a. Coercible a (u a)
  => Convertor u 'FromSI p a -> a -> u a
fromSI u = coerce (runConvertor u)
{-# INLINE fromSI #-}


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


type family PushNeg (u :: Unit) :: Unit where
  PushNeg (u -^- Neg n -*- v) = PushNeg u -^- Pos n


type family ElimDiv (u :: Unit) :: Unit where
    ElimDiv (u -*- v) = ElimDiv u -*- ElimDiv v
    ElimDiv (u -^- n) = ElimDiv u -^- n
    ElimDiv (u -/- u) = NoUnit
    ElimDiv (u -/- v) = ElimDiv u -*- ElimDiv v -^- Neg 1
    ElimDiv u = u

type family ElimPow (u :: Unit) :: Unit where
  ElimPow (u -/- v) = ElimPow u -/- ElimPow v
  ElimPow (u -*- v) = ElimPow u -*- ElimPow v
  ElimPow (u -^- Pos 0) = NoUnit
  ElimPow (u -^- Neg 0) = NoUnit
  ElimPow (u -^- Pos 1) = ElimPow u
  ElimPow ((u -^- n) -^- m) = ElimPow (u -^- (n `MulRel` m))
  ElimPow ((u -*- v) -^- n) = ElimPow (u -^- n -*- v -^- n)
  ElimPow u = u

type family ElimNoUnit (u :: Unit) :: Unit where
  ElimNoUnit (NoUnit -*- u) = ElimNoUnit u
  ElimNoUnit (u -*- NoUnit) = ElimNoUnit u
  ElimNoUnit (u -*- v) = ElimNoUnit u -*- ElimNoUnit v
  ElimNoUnit u = u

type family SimplifyUnit (u :: Unit) :: Unit where
  SimplifyUnit u = ElimNoUnit (ElimPow (ElimDiv u))



type family UnitToSI (u :: Unit) :: Unit where
  UnitToSI u = SimplifyUnit (DimToSI (ToDim u))


--  | This does NOT work ! Type inference will fail as soon as there is a negative exponent.
toSI :: forall u cd p a. Coercible a ((UnitToSI u) a)
  => Convertor u 'ToSI 'False a -> a -> (UnitToSI u) a
toSI u = coerce (runConvertor u)
{-# INLINE toSI #-}

asSI :: forall u a.
  (Coercible a (u a), Coercible a ((UnitToSI u) a)
  , ConvertorClass u 'ToSI 'False a)
  =>  u a -> (UnitToSI u) a
asSI fa = toSI (convertor :: Convertor u 'ToSI 'False a ) (coerce fa :: a)
{-# INLINE asSI #-}

fromSI' :: Convertor u 'FromSI 'False a  -> a -> a
fromSI' = runConvertor
{-# INLINE fromSI' #-}

toSI' :: Convertor u 'ToSI 'False a -> a -> a
toSI'  = runConvertor
{-# INLINE toSI' #-}



fromToNoCheck :: forall u v a. (Coercible a (v a), Coercible a (u a))
   => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
fromToNoCheck u v a = coerce
   $ runConvertor v
   $ runConvertor u
   $ (coerce a :: a)
{-# INLINE fromToNoCheck #-}

fromTo :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim u v)
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
fromTo = fromToNoCheck
{-# INLINE fromTo #-}

fromToNoCheck' :: forall u v a.
  Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> a -> a
fromToNoCheck' u t  = fromSI' t . toSI' u
{-# INLINE fromToNoCheck' #-}

fromTo' ::  forall u v a. SameDim u v
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> a -> a
fromTo' = fromToNoCheck'
{-# INLINE fromTo' #-}
infix 2 `fromTo'`




(~>) :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim u v)
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall u v a. SameDim u v
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> a -> a
(~~>) = fromTo'
{-# INLINE (~~>) #-}
infix 2 ~~>

asNoCheck :: forall u v a.
  (Coercible a (u a), Coercible a (v a), ConvertorClass u 'ToSI 'False a)
  => u a -> Convertor v 'FromSI 'False a  -> v a
asNoCheck fa v = fromToNoCheck (convertor :: Convertor u 'ToSI 'False a ) v fa
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
as :: forall u v a.
  (Coercible a (u a), Coercible a (v a)
  , ConvertorClass u 'ToSI 'False a, SameDim u v)
  => u a -> Convertor v 'FromSI 'False a  -> v a
as = asNoCheck
{-# INLINE as #-}
infix 2 `as`


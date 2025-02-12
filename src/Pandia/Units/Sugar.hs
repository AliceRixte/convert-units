{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Pandia.Units.Sugar
  ( module Pandia.Units.Sugar
  ) where

import Data.Coerce

import Pandia.Units.Dimension
import Pandia.Units.Convertor
import Pandia.Units.System
import Pandia.Units.Unit



type family UnitToSI (u :: Unit) :: Unit where
  UnitToSI u = SimplifyUnit (DimToBaseUnit (DimOf SI u))

fromSI :: forall u a. Coercible a (u a)
  => FromSys u a -> a -> u a
fromSI u = coerce (runConvertor u)
{-# INLINE fromSI #-}


--  | This does NOT work ! Type inference will fail as soon as there is a negative exponent.
toSI :: forall u a. Coercible a ((UnitToSI u) a)
  => ToSys u a -> a -> (UnitToSI u) a
toSI u = coerce (runConvertor u)
{-# INLINE toSI #-}

asSI :: forall u a.
  (Coercible a (u a), Coercible a ((UnitToSI u) a)
  , ConvertorClass u 'ToDimSys 'False a)
  =>  u a -> (UnitToSI u) a
asSI fa = toSI (convertor :: ToSys u a ) (coerce fa :: a)
{-# INLINE asSI #-}

-- fromSys' :: FromSys u a  -> a -> a
-- fromSys' = runConvertor
-- {-# INLINE fromSys' #-}

-- toSys' :: ToSys u a -> a -> a
-- toSys'  = runConvertor
-- {-# INLINE toSys' #-}





fromTo :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim SI u v)
  => ToSys u a -> FromSys v a  -> u a -> v a
fromTo = convertNoCheck
{-# INLINE fromTo #-}


fromTo' ::  forall u v a. SameDim SI u v
  => ToSys u a -> FromSys v a  -> a -> a
fromTo' = convertNoCheck'
{-# INLINE fromTo' #-}
infix 2 `fromTo'`





(~>) :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim SI u v)
  => ToSys u a -> FromSys v a  -> u a -> v a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall u v a. SameDim SI u v
  => ToSys u a -> FromSys v a  -> a -> a
(~~>) = fromTo'
{-# INLINE (~~>) #-}
infix 2 ~~>




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
  , ConvertorClass u 'ToDimSys 'False a, SameDim SI u v)
  => u a -> FromSys v a  -> v a
as = asNoCheck
{-# INLINE as #-}
infix 2 `as`


{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Pandia.Units.AngleSI
  ( module Pandia.Units.AngleSI
  ) where

import Data.Coerce

import Pandia.Units.Internal.Core


type family UnitToASI (u :: Unit) :: Unit where
  UnitToASI u = SimplifyUnit (DimToBaseUnit (DimOf AngleSI u))

fromASI :: forall u a. Coercible a (u a)
  => FromSys u a -> a -> u a
fromASI u = coerce (runConvertor u)
{-# INLINE fromASI #-}


--  | This does NOT work ! Type inference will fail as soon as there is a negative exponent.
toASI :: forall u a. Coercible a ((UnitToASI u) a)
  => ToSys u a -> a -> (UnitToASI u) a
toASI u = coerce (runConvertor u)
{-# INLINE toASI #-}

asASI :: forall u a.
  (Coercible a (u a), Coercible a ((UnitToASI u) a)
  , ConvertorClass u 'ToDimSys 'False a)
  =>  u a -> (UnitToASI u) a
asASI fa = toASI (convertor :: ToSys u a ) (coerce fa :: a)
{-# INLINE asASI #-}


fromTo :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim AngleSI u v)
  => ToSys u a -> FromSys v a  -> u a -> v a
fromTo = convertNoCheck
{-# INLINE fromTo #-}


fromTo' ::  forall u v a. SameDim AngleSI u v
  => ToSys u a -> FromSys v a  -> a -> a
fromTo' = convertNoCheck'
{-# INLINE fromTo' #-}
infix 2 `fromTo'`


(~>) :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim AngleSI u v)
  => ToSys u a -> FromSys v a  -> u a -> v a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall u v a. SameDim AngleSI u v
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
  , ConvertorClass u 'ToDimSys 'False a, SameDim AngleSI u v)
  => u a -> FromSys v a  -> v a
as = asNoCheck
{-# INLINE as #-}
infix 2 `as`


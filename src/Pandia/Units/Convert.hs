{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Pandia.Units.Convert
  ( module Pandia.Units.Convert
  ) where

import Data.Coerce

import Pandia.Units.Dimension
import Pandia.Units.Convertor
import Pandia.Units.SI
import Pandia.Units.Unit



type family UnitToSI (u :: Unit) :: Unit where
  UnitToSI u = SimplifyUnit (DimToBaseUnit (DimOf SI u))

fromSI :: forall u a. Coercible a (u a)
  => Convertor u 'FromSI 'False a -> a -> u a
fromSI u = coerce (runConvertor u)
{-# INLINE fromSI #-}


--  | This does NOT work ! Type inference will fail as soon as there is a negative exponent.
toSI :: forall u a. Coercible a ((UnitToSI u) a)
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
  (Coercible a (v a), Coercible a (u a), SameDim SI u v)
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
fromTo = fromToNoCheck
{-# INLINE fromTo #-}

fromTo2 :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim SI u v)
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
fromTo2 = fromToNoCheck
{-# INLINE fromTo2 #-}


-- fromTo2 :: forall u v a.
--   (Coercible a (v a), Coercible a (u a), SameDim SI u v)
--   => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
-- fromTo = fromToNoCheck
-- {-# INLINE fromTo #-}

fromToNoCheck' :: forall u v a.
  Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> a -> a
fromToNoCheck' u t  = fromSI' t . toSI' u
{-# INLINE fromToNoCheck' #-}

fromTo' ::  forall u v a. SameDim SI u v
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> a -> a
fromTo' = fromToNoCheck'
{-# INLINE fromTo' #-}
infix 2 `fromTo'`




(~>) :: forall u v a.
  (Coercible a (v a), Coercible a (u a), SameDim SI u v)
  => Convertor u 'ToSI 'False a -> Convertor v 'FromSI 'False a  -> u a -> v a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall u v a. SameDim SI u v
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
  , ConvertorClass u 'ToSI 'False a, SameDim SI u v)
  => u a -> Convertor v 'FromSI 'False a  -> v a
as = asNoCheck
{-# INLINE as #-}
infix 2 `as`


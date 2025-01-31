{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Pandia.Units.Convert
  ( module Pandia.Units.Convert
  ) where

import Data.Coerce
import Data.Proxy


import Pandia.Units.Dimension
import Pandia.Units.Convertor


fromSI :: forall f a. Coercible a (f a)
  => Convertor f (From a) -> a -> f a
fromSI f a = coerce (f (Proxy :: Proxy f)) a

toSI :: forall f a. Coercible a (f a)
  => Convertor f (To a) -> a -> f a
toSI f a = coerce (f (Proxy :: Proxy f)) a

fromSI' :: forall f a. Coercible a (f a)
  => Convertor f (From a) -> a -> a
fromSI' f a = coerce (f (Proxy :: Proxy f)) a

toSI' :: forall f a. ConvertorClass f (To a)
  => Convertor f (To a) -> a -> a
toSI' f a = coerce (f (Proxy :: Proxy f)) a




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
fromToNoCheck' f t  = (coerce (t (Proxy :: Proxy g)) :: a -> a)
                    . (coerce (f (Proxy :: Proxy f)) :: a -> a)



asNoCheck :: forall f g a.
  (Coercible a (f a), Coercible a (g a), ConvertorClass f (From a))
  => f a -> Convertor g (To a) -> g a
asNoCheck fa g = fromToNoCheck (convertor :: Convertor f (From a)) g fa
{-# INLINE asNoCheck #-}


(~>) :: forall f g a.
  (Coercible a (g a), Coercible a (f a), SameDim f g)
  => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall f g a.
  Convertor f (From a) -> Convertor g (To a) -> a -> a
f ~~> t = (coerce (t (Proxy :: Proxy g)) :: a -> a)
            . (coerce (f (Proxy :: Proxy f)) :: a -> a)
{-# INLINE (~~>) #-}
infix 2 ~~>




convertCheck :: forall f g a.
  (Coercible a (g a), Coercible a (f a), SameDim f g)
  => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
convertCheck = (~>)

-- | Convert a quantity from one unit to the other, and checks if the unit's
-- dimensions are compatible
--
-- @
-- >>> x = 4 :: (Kilo Meter -/- Second) Double
-- >>> asCheck x meter
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


{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Units.Base.Convert
  ( ConvFactor(..)
  , From(..)
  , To(..)
  , FromTo
  , fromTo
  , FromTo'
  , fromTo'
  , from'
  , to'
   -- Remove
  , DimEq
  , fromCoerce
  , toCoerce
  , fromToCoerce
  )
  where
import Data.Kind
import Data.Coerce
import Data.Proxy


import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits


import Data.Type.Int

import Data.Units.Base.Dimension
import Data.Units.Base.Unit



-- | A unit whose quantities are convertible from that unit to its corresponding
-- standard unit.
class (IsUnit u, IsUnit (StdUnitOf u)) => From u a where
  -- Conversion from targeted unit to its corresponding standard unit.
  --
  -- >>> import Data.Units.NonStd.Time
  -- >>> from (as @(Kilo Meter -/- Hour) 1)
  --
  from :: u a -> StdUnitOf u a

fromCoerce :: forall u a. From u a => a -> a
fromCoerce = unQuantity @(StdUnitOf u) . from . quantity @u
{-# INLINE fromCoerce #-}

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, IsUnit (StdUnitOf u), IsUnit u)
    => From u a where
  from = from'

class (IsUnit u, IsUnit (StdUnitOf u)) => To u a where
  to :: StdUnitOf u a -> u a

toCoerce :: forall u a. To u a => a -> a
toCoerce = unQuantity @u . to . quantity @(StdUnitOf u)

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, IsUnit (StdUnitOf u), IsUnit u)
  => To u a where
  to = to'
  {-# INLINE to #-}

type FromTo u v a = (DimEq u v, From u a, To v a)

fromTo :: FromTo u v a => u a -> v a
fromTo = to . from

fromToCoerce :: forall u v a. FromTo u v a => a -> a
fromToCoerce = unQuantity @v . fromTo . quantity @u

--------------------------------------------------------------------------------

class (From u a, To u a, Fractional a) => ConvFactor u a where
  {-# MINIMAL factorFrom | factorTo #-}
  factorFrom :: a
  factorFrom = 1 / factorTo @u
  {-# INLINE factorFrom #-}

  factorTo :: a
  factorTo = 1 / factorFrom @u
  {-# INLINE factorTo #-}

instance (IsUnit u, Fractional a) => ConvFactor (MetaUnit u) a where
  factorFrom = 1
  {-# INLINE factorFrom #-}

instance Fractional a => ConvFactor NoUnit a where
  factorFrom = 1
  {-# INLINE factorFrom #-}

instance (Num a, ConvFactor u a, ConvFactor v a, IsUnit (StdUnitOf (u-*- v)))
  =>  ConvFactor (u -*- v) a where
  factorFrom = factorFrom @u * factorFrom @v
  {-# INLINE factorFrom #-}

instance (ConvFactor u a, IsUnit (StdUnitOf (u -^- n)),  KnownInt n)
  =>  ConvFactor (u -^- n) a where
  factorFrom = factorFrom @u ^^ intVal (Proxy :: Proxy n)
  {-# INLINE factorFrom #-}


type family DimEq (u :: Unit) (v :: Unit) :: Constraint where
  DimEq u v =
    ( IsUnit u
    , IsUnit v
    , StdUnitOf u ~ StdUnitOf v
    , IsUnit (StdUnitOf u)
    , If (StdUnitOf u == StdUnitOf v) (() :: Constraint)
      (TypeError (
          Text "Cannot convert unit ‘"
          :<>: ShowUnitType u
          :<>: Text "’ to unit ‘"
          :<>: ShowUnitType v
          :<>: Text "’ because their dimensions do not match."
          :$$: Text "Dimension of ‘"
          :<>: ShowUnitType u
          :<>: Text "’ is: "
          :<>: ShowDim (DimOf (StdUnitOf u))
          :$$: Text "Dimension of ‘"
          :<>: ShowUnitType v
          :<>: Text "’ is: "
          :<>: ShowDim (DimOf (StdUnitOf v))
    )))


from' :: forall u a. (ConvFactor u a, Coercible a (StdUnitOf u a))
  => u a -> StdUnitOf u a
from' q = quantity (unQuantity q * factorFrom @u)

to' :: forall u a. (ConvFactor u a, Coercible a (StdUnitOf u a))
  => StdUnitOf u a -> u a
to' q = quantity (unQuantity q * factorTo @u)

type FromTo' u v a = (DimEq u v, ConvFactor u a, ConvFactor v a)
fromTo' :: forall u v a.
  FromTo' u v a
  => u a -> v a
fromTo' q = quantity (unQuantity q * (factorFrom @u * factorTo @v))




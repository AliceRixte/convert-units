{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Units.Base.Convert
  ( module Data.Units.Base.Convert
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


class (IsUnit u, IsUnit (StdUnitOf u)) => From u a where
  from :: u a -> StdUnitOf u a

fromCoerce :: forall u a. From u a => a -> a
fromCoerce a = coerce $ from (coerce a :: u a)
{-# INLINE fromCoerce #-}

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, IsUnit (StdUnitOf u), IsUnit u)
    => From u a where
  from = from'

class (IsUnit u, IsUnit (StdUnitOf u)) => To u a where
  to :: StdUnitOf u a -> u a

toCoerce :: forall u a. To u a => a -> a
toCoerce a = coerce (to (coerce a :: StdUnitOf u a) :: u a)

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, IsUnit (StdUnitOf u), IsUnit u)
  => To u a where
  to = to'
  {-# INLINE to #-}

type Convertible u v a = (DimEq u v, From u a, From v a, To u a, To v a)
type FromTo u v a = (DimEq u v, From u a, To v a)

fromTo :: FromTo u v a => u a -> v a
fromTo = to . from

fromToCoerce :: forall u v a. FromTo u v a => a -> a
fromToCoerce a = coerce (fromTo (coerce a :: u a) :: v a)


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
from' q = coerce (coerce q * factorFrom @u :: a)

to' :: forall u a. (ConvFactor u a, Coercible a (StdUnitOf u a))
  => StdUnitOf u a -> u a
to' q = coerce (coerce q * factorTo @u :: a)

type FromTo' u v a = (DimEq u v, ConvFactor u a, ConvFactor v a)
fromTo' :: forall u v a.
  FromTo' u v a
  => u a -> v a
fromTo' q = coerce (coerce q * (factorFrom @u * factorTo @v) :: a)




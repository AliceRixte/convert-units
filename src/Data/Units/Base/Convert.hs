{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Units.Base.Convert where
import Data.Kind
import Data.Coerce
import Data.Proxy


import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits


import Data.Type.Int

import Data.Convert.FromTo
import Data.Units.Base.Dimension
import Data.Units.Base.Unit

class (IsUnit u, Fractional a) => ConvFactor u a where
  factorFrom :: a
  factorFrom = 1 / factorTo @u
  {-# INLINE factorFrom #-}

  factorTo :: a
  factorTo = 1 / factorFrom @u
  {-# INLINE factorTo #-}

instance Fractional a => ConvFactor (StdUnit u) a where
  factorFrom = 1

instance Fractional a => ConvFactor NoUnit a where
  factorFrom = 1

instance (Num a, ConvFactor u a, ConvFactor v a)
  =>  ConvFactor (u -*- v) a where
  factorFrom = factorFrom @u * factorFrom @v

instance (ConvFactor u a, KnownInt n)
  =>  ConvFactor (u -^- n) a where
  factorFrom = factorFrom @u ^^ intVal (Proxy :: Proxy n)

type SameStdUnit u v = (StdUnitOf u ~ StdUnitOf v, DimEq u v)

type family DimEq (u :: Unit) (v :: Unit) :: Constraint where
  DimEq u v = If (StdUnitOf u == StdUnitOf v) (() :: Constraint)
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
    ))


from' :: forall u a. (ConvFactor u a, Coercible a (StdUnitOf u a))
  => u a -> StdUnitOf u a
from' q = coerce (coerce q * factorFrom @u :: a)

to' :: forall u a. (ConvFactor u a, Coercible a (StdUnitOf u a))
  => StdUnitOf u a -> u a
to' q = coerce (coerce q * factorTo @u :: a)

convertUnit :: forall u v a.
  (SameStdUnit u v, IsUnit u, From (u a), IsUnit v, To (v a))
  => u a -> v a
convertUnit = to . from

convertUnit' :: forall u v a.
  (DimEq u v, ConvFactor u a, ConvFactor v a)
  => u a -> v a
convertUnit' q = coerce (coerce q * (factorFrom @u * factorTo @v) :: a)

--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} IsUnit u => HasStandard (u a) where
  type StandardOf (u a) = StdUnitOf u a

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, Coercible a (StdUnitOf u a))
  => From (u a) where
  from = from'

instance {-# OVERLAPPABLE #-}
  (ConvFactor u a, Coercible a (StdUnitOf u a))
  => To (u a) where
  to = to'


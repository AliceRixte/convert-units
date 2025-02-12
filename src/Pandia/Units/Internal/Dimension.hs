{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds#-}

module Pandia.Units.Internal.Dimension
  ( module Pandia.Units.Internal.Dimension
  ) where

import Pandia.Units.Internal.Rel
import Pandia.Units.Internal.Unit

import GHC.TypeLits

import Data.Kind

data Dim k = Dim k Rel

type family ElimNegZero (l :: [Dim k]) :: [Dim k] where
  ElimNegZero '[] = '[]
  ElimNegZero ('Dim k n ': ds) = 'Dim k (NormalizeRel n) ': ElimNegZero ds


-- | Print both dimensions in case of error
data DimCheck d =
    DimOK
  | DimError d d

type family EqAllDim (l :: [Dim k]) (l' :: [Dim k]) :: Bool where
  EqAllDim '[] '[] = 'True
  EqAllDim (d : ds) (d ': ds') = EqAllDim ds ds'

type family PrettyFail (d :: [Dim k]) (d' :: [Dim k]) (b :: Bool)
  :: DimCheck [Dim k] where
  PrettyFail _ _ 'True = 'DimOK
  PrettyFail d d' 'False = 'DimError d d'

type family EqDim (l :: [Dim k]) (l' :: [Dim k]) :: DimCheck [Dim k] where
  EqDim l l' = PrettyFail l l' (EqAllDim l l')





type family MulOneDim (d :: Dim k) (d' :: Dim k)
  :: Dim k
  where
  MulOneDim ('Dim k m) ('Dim k n) = 'Dim k (m `SumRel` n)

type family MulDim' (l :: [Dim k]) (d' :: [Dim k])
  :: [Dim k]
  where
  MulDim' '[] '[] = '[]
  MulDim' (d ': ds) (d' ': ds') = MulOneDim d d' ': MulDim' ds ds'

type family DivOneDim (d :: Dim k) (d' :: Dim k)
  :: Dim k
  where
  DivOneDim ('Dim k m) ('Dim k n) = 'Dim k (m `SubRel` n)

type family DivDim' (l :: [Dim k]) (d' :: [Dim k])
  :: [Dim k]
  where
  DivDim' '[] '[] = '[]
  DivDim' (d ': ds) (d' ': ds') = DivOneDim d d' ': DivDim' ds ds'

type family PowOneDim (d :: Dim k) (n :: Rel)
  :: Dim k
  where
  PowOneDim ('Dim k m) n = 'Dim k (m `MulRel` n)

type family PowDim' (l :: [Dim k]) (n :: Rel)
  :: [Dim k]
  where
  PowDim' '[] n = '[]
  PowDim' (d ': ds) n = PowOneDim d n ': PowDim' ds n

------------------------------------------------------------------------------

type DimSystem k = [Rel -> Dim k]

type family DimensionLess (l :: DimSystem k) :: [Dim k] where
  DimensionLess '[] = '[]
  DimensionLess (f ': ds) = f (Pos 0) ': DimensionLess ds

type family SetDim (key :: k) (n::Rel) (l :: [Dim k]) :: [Dim k] where
  SetDim k n ('Dim k _ ': ds) = 'Dim k n ': ds
  SetDim k n (d ': ds) = d : SetDim k n ds

type family MonoDim (syst :: DimSystem k) (key :: k) (n :: Rel) where
  MonoDim syst key n = SetDim key n (DimensionLess syst)




class HasDim (syst :: DimSystem Symbol) (u::Unit) where
  type DimOf syst u :: [Dim Symbol]

instance HasDim syst NoUnit where
  type DimOf syst NoUnit = DimensionLess syst

instance (HasDim syst u, HasDim syst v) => HasDim syst (u -*- v) where
  type DimOf syst (u -*- v) = MulDim' (DimOf syst u) (DimOf syst v)

instance (HasDim syst u, HasDim syst v) => HasDim syst (u -/- v) where
  type DimOf syst (u -/- v) = DivDim' (DimOf syst u) (DimOf syst v)

instance (HasDim syst u) => HasDim syst (u -^- n) where
  type DimOf syst (u -^- n) = PowDim' (DimOf syst u) n


type SameDim syst u v = EqDim (DimOf syst u) (DimOf syst v) ~ 'DimOK


type family BaseUnit (k :: Symbol) :: Unit
type family FlexQuantity (k :: Symbol) :: Type -> Type

type family DimToBaseUnit (d :: [Dim k]) :: Unit where
  DimToBaseUnit '[] = NoUnit
  DimToBaseUnit ('Dim k n ': ds) = BaseUnit k -^- n -*- DimToBaseUnit ds


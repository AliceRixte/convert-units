{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Units.Base.Constructor
  ( module Data.Units.Base.Constructor
  , Unit
  ) where

import Data.Kind
import Data.Coerce
import Data.Proxy

import Data.Type.Ord
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits


import Data.Type.Int

import Data.Convert.FromTo
import Data.Units.Base.Dimension
import Data.Units.Base.Convert
import Data.Units.Base.Quantity
import Data.Units.Base.Unit



class From u where
  from :: u -> Standard u

instance {-# OVERLAPPABLE #-}
  (Fractional a, IsQuantity q a, Coercible a (Standard q))
  => From q where
  from u = coerce $ coerce u * convFactor @q

class To a where
  to :: Standard a -> a

instance {-# OVERLAPPABLE #-}
  (Fractional a, IsQuantity q a, Coercible a (Standard q))
  => To q where
  to u = coerce $ coerce u / convFactor @q

-- to' :: (Fractional a, IsQuantity q a, Coercible a (Standard q))
--   => Standard q
-- to' q = coerce $ coerce u / convFactor @q

convert :: (Standard a ~ Standard b, From a, To b) => a -> b
convert = to . from


type family Standard q where
  Standard (u a) = (NormalizeUnit u) a

-- type instance Standard (NoUnit a) =  NoUnit a

-- type instance Standard ((u -*- v) a) =
--   NormalizeUnit (ApplyStandard ((u -*- v) a)) a

-- type instance Standard ((u -^- n) a) =
--   NormalizeUnit (ApplyStandard ((u -^- n) a)) a




----------------------------- Unit construction ------------------------------





-- | Multiply two quantities
--
(-*-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Num a)
 => u a -> v a -> (u -*- v) a
u -*- v = coerce (coerce u * coerce v :: a)
{-# INLINE (-*-) #-}







-- | Divide two quantities
--
(-/-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Fractional a)
  => u a -> v a -> (u -/- v) a
u -/- v = coerce (coerce u / coerce v :: a)
{-# INLINE (-/-) #-}











type a -^+ b = a -^- Pos b
infix 8 -^+

type a -^~ b = a -^- Neg b
infix 8 -^~

-- | Raise a quantity to a power
--
(-^-) :: forall u n a. (Coercible a (u a), Num a)
  => u a -> Proxy n -> (u -^- n) a
u -^- _ = coerce u
{-# INLINE (-^-) #-}



--------------------------------------------------------------------------------

type family ApplyStandard u :: Unit where
  ApplyStandard ((u -*- v) a) = ApplyStandard (u a) -*- ApplyStandard (v a)
  ApplyStandard ((u -^- n) a) = ApplyStandard (u a) -^- n
  ApplyStandard (u a) = GetUnitCons (Standard (u a))
  ApplyStandard u = TypeError (
    Text "The type family ApplyStandard should be called with a unit 'u a'"
    :$$: Text "but '"
    :<>: ShowType u
    :<>: Text "' is not of the form 'u a'."
    )

type family GetUnitCons u :: Unit where
  GetUnitCons (u a) = u
  GetUnitCons u = TypeError (
    Text "The type family GetUnitCons should be called with a unit 'u a'"
    :$$: Text "but '"
    :<>: ShowType u
    :<>: Text "' is not of the form 'u a'."
    )


--------------------------------------------------------------------------------






--------------------------------------------------------------------------------

type family DimEq u v :: Constraint where
  DimEq u v = If (Standard u == Standard v) (() :: Constraint)
    (TypeError (
        Text "Cannot convert unit "
        :<>: ShowType (GetUnitCons u)
        :<>: Text " to unit "
        :<>: ShowType (GetUnitCons v)
        :<>: Text " because their dimensions do not match."
        :$$: Text "Dimension of "
        :<>: ShowType (GetUnitCons u)
        :<>: Text " is: "
        :<>: Text (DimName (DimOf (GetUnitCons (Standard u))))
        :$$: Text "Dimension of "
        :<>: ShowType (GetUnitCons v)
        :<>: Text " is: "
        :<>: Text (DimName (DimOf (GetUnitCons (Standard v))))
    ))



convert' :: forall u v a. (IsQuantity (u a) a, IsQuantity (v a) a,
  DimEq (Standard (u a)) (Standard (v a)), Fractional a) =>
  u a -> v a
convert' u = coerce $ (coerce u :: a) * (convFactor @(u a) / convFactor @(v a))



{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

------------------------------------------------------------------------------
-- | A template convertor for personalized independant units.
--
-- The type of @'myunit'@ can be specialized by using @'myunit' :: Convertor MyUnit a@
--
-- @
-- newtype Frame a = Frame a deriving (Show, Eq, Num)
-- type FPS = Frame -/- Second
-- instance ConvertorClass Frame a
-- frame = convertor :: Convertor Frame a
-- fps = frame `per` second
-- @
--
-- @
-- >>> decaRate = 2 :: (Deca Frame -/- Second) Int
-- >>> decaRate `as` fps
-- PerDim (Frame (Second 20))
-- >>> deca frame `per` second ~> fps) decaRate
-- PerDim (Frame (Second 20))
-- @
--
-- It is important to notice that this only works for new independant units.
--
------------------------------------------------------------------------------

module Pandia.Units.Convertor
  ( module Pandia.Units.Convertor
  ) where

import Data.Coerce
import Data.Proxy
import Data.Kind
import Data.Functor.Identity

import GHC.TypeLits




-- | Multiplication of two units.
--
-- @
-- type MyForceMoment a = (Newton -*- Meter) a
-- @
--
newtype ((f :: Type -> Type) -*- (g :: Type -> Type)) a = MulDim (f (g a))
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infixl 7 -*-

-- | Division of two units.
--
-- @
-- type MySpeed a = (Meter -/- Second) a
-- type MyMolarEntropy a = (Joule -/- Mole -*- Kelvin) a
-- @
--
-- Notice that division has priority over division.
--
newtype ((f :: Type -> Type) -/- (g :: Type -> Type)) a = PerDim (f (g a))
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infix 6 -/-

-- | Unit to the power of a positive natural number
--
-- Negative exponents are not supported. Use division and @'NoUnit'@ if you need
-- them.
--
-- @
-- type MyAcceleration a = (Meter -/- Second -^- 2) a
-- @
--
newtype ((f :: Type -> Type) -^- (n :: Nat)) a = PowDim (f a)
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)
infix 8 -^-

-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit -/- Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)

-- | A convertor that can convert from and to some unit.
--
-- Convertors can be combined via 'mul'@, @'per'@, and @'pow'@.
--
type Convertor (f :: Type -> Type) a = Proxy f -> a -> a

-- | Create a convertor from a unit newtype
--
class ConvertorClass (f :: Type -> Type) a where
  convertor :: Convertor f a
  convertor _ = id
  {-# INLINE convertor #-}

instance (ConvertorClass f a, ConvertorClass g (Per a), Num a)
  => ConvertorClass ( f -/- g) a where
  convertor =
    (convertor :: Convertor f a) -/- (convertor :: Convertor g (Per a))
  {-# INLINE convertor #-}

instance (ConvertorClass f a, ConvertorClass g a, Num a)
  => ConvertorClass ( f -*- g) a where
  convertor =
    (convertor :: Convertor f a) -*- (convertor :: Convertor g a)
  {-# INLINE convertor #-}

instance (PowClass f n a, ConvertorClass f a, KnownNat n)
  => ConvertorClass ( f -^- n) a where
  convertor =
    (convertor :: Convertor f a) -^- fromInteger (natVal (Proxy :: Proxy n))
  {-# INLINE convertor #-}


-- | When a quantity decorated by this newtype is fed to a convertor, the
-- convertor will compute the conversion from its unit to the international
-- system unit
newtype From a = From a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

-- | When a quantity decorated by this newtype is fed to a convertor, the
-- convertor will compute the conversion from the international system unit to
-- its unit
newtype To a = To a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

-- | When receiving a quantity of the form @'Per' ('From' a)@, the convertor
-- will compute the conversion from the international system unit to its
-- inverted unit
--
-- Similarly, when receiving a quantity of the form @'Per' ('To' a)@, the
-- convertor will compute the conversion from its unit to the international
-- system unit
--
newtype Per a = Per a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)


-- | A convertor that does nothing
--
-- @
-- >>> (nounit `per` milli seconds ~~> hertz) (5 :: Int)
-- 5000
-- @
--
nounit :: Convertor NoUnit a
nounit _ = id
{-# INLINE nounit #-}


-- | Division of convertors
--
-- @
-- >>> (kilo meter `per` hour ~~> meter `per` second ) (5 :: Float)
-- 1.388889
-- @
--
per :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
per f g _ a = f (Proxy :: Proxy f) a *
              (coerce (g (Proxy :: Proxy g)) :: a -> a) 1
infix 6 `per`

-- | Infix synonym for @'per'@
--
-- @
-- >>> (kilo meter -/- hour ~~> meter -/- second ) (5 :: Float)
-- 1.388889
-- @
--
(-/-) :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
(f -/- g) a = per f g a
{-# INLINE (-/-) #-}


-- | Multiplication of convertors
--
-- @
-- >>> (newton `mul` kilo meter  ~~> newton `mul` meter ) (5 :: Float)
-- 5000
mul :: forall f g a. Num a
  => Convertor f a -> Convertor g a -> Convertor (f -*- g) a
mul f g _ a = f (Proxy :: Proxy f) a * g (Proxy :: Proxy g) a
{-# INLINE mul #-}
infixl 7 `mul`


(-*-) :: forall f g a. Num a =>
  Convertor f a -> Convertor g a -> Convertor (f -*- g) a
(f -*- g) _ a = f (Proxy :: Proxy f) a * g (Proxy :: Proxy g) a
{-# INLINE (-*-) #-}


class PowClass (f :: Type -> Type) (n::Nat) a where
  pow :: Convertor f a -> Int -> Convertor (f -^- n) a
  infix 8 `pow`

instance PowClass f 0 a where
  pow _ 0 _ = coerce (id :: a -> a)
  pow _ _ _ = error "The exponent doesn't match the dimension"
  {-# INLINE pow #-}

instance PowClass f 1 a where
  pow f 1 = coerce f
  pow _ _ = error "The exponent doesn't match the dimension"
  {-# INLINE pow #-}

instance (PowClass f n a, np1 ~ n + 1, Num a) => PowClass f np1 a where
  pow f n _ a = f (Proxy :: Proxy f) a
                * pow f (n - 1) (Proxy :: Proxy (f -^- n)) a
  {-# INLINE pow #-}

(-^-) :: forall f n a.  PowClass f n a
  => Convertor f a -> Int -> Convertor (f -^- n) a
(-^-) = pow





fromTo :: forall f g a. (Coercible a (g a), Coercible a (f a))
   => Convertor f (From a) -> Convertor g (To a) -> f a -> g a
fromTo f t a = coerce
   $ (coerce (t (Proxy :: Proxy g)) :: a -> a)
   $ (coerce (f (Proxy :: Proxy f)) :: a -> a)
   $ (coerce a :: a)
{-# INLINE fromTo #-}
infix 2 `fromTo`


as :: forall f g a.
  (Coercible a (f a), Coercible a (g a), ConvertorClass f (From a))
  => f a -> Convertor g (To a) -> g a
as fa g = fromTo (convertor :: Convertor f (From a)) g fa
{-# INLINE as #-}
infix 2 `as`

(~>) :: forall f g a. (Coercible a (g a), Coercible a (f a))
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


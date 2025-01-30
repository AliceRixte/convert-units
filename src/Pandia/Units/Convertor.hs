{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Pandia.Units.Convertor
  ( module Pandia.Units.Convertor
  ) where

import Data.Coerce
import Data.Proxy
import Data.Kind

import GHC.TypeLits



type Convertor (f :: Type -> Type) a = Proxy f -> a -> a

newtype From a = From a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

newtype To a = To a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

newtype Per a = Per a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)


newtype ((f :: Type -> Type) -/- (g :: Type -> Type)) a = PerDim (f (g a))
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)
infix 6 -/-

newtype ((f :: Type -> Type) -*- (g :: Type -> Type)) a = MulDim (f (g a))
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)
infixl 7 -*-

newtype ((f :: Type -> Type) -^- (n :: Nat)) a = PowDim (f a)
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Functor)
infixl 8 -^-


class ConvertorClass (f :: Type -> Type) a where
  convertor :: Convertor f a

instance (ConvertorClass f a, ConvertorClass g (Per a), Num a)
  => ConvertorClass ( f -/- g) a where
  convertor =
    (convertor :: Convertor f a) -/- (convertor :: Convertor g (Per a))

instance (ConvertorClass f a, ConvertorClass g a, Num a)
  => ConvertorClass ( f -*- g) a where
  convertor =
    (convertor :: Convertor f a) -*- (convertor :: Convertor g a)

instance (PowDimClass f n a, ConvertorClass f a, KnownNat n)
  => ConvertorClass ( f -^- n) a where
  convertor =
    (convertor :: Convertor f a) -^- fromInteger (natVal (Proxy :: Proxy n))


per :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
per f g _ a = f (Proxy :: Proxy f) a *
              (coerce (g (Proxy :: Proxy g)) :: a -> a) 1
infix 6 `per`

(-/-) :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
(f -/- g) a = per f g a
{-# INLINE (-/-) #-}

(-*-) :: forall f g a. Num a =>
  Convertor f a -> Convertor g a -> Convertor (f -*- g) a
(f -*- g) _ a = f (Proxy :: Proxy f) a * g (Proxy :: Proxy g) a
{-# INLINE (-*-) #-}


class PowDimClass (f :: Type -> Type) (n::Nat) a where
  powDim :: Convertor f a -> Int -> Convertor (f -^- n) a

instance PowDimClass f 1 a where
  powDim f n | n == 1 = coerce f
  powDim _ _ = error "This is impossible. Pleas report this as a bug."

instance (PowDimClass f n a, np1 ~ n + 1, Num a) => PowDimClass f np1 a where
  powDim f n _ a = f (Proxy :: Proxy f) a
                * powDim f (n - 1) (Proxy :: Proxy (f -^- n)) a
  {-# INLINE powDim #-}


(-^-) :: forall f n a.  PowDimClass f n a
  => Convertor f a -> Int -> Convertor (f -^- n) a
(-^-) = powDim



fromTo :: forall f g a. Coercible a (g a)
   => Convertor f (From a) -> Convertor g (To a) -> g a -> g a
fromTo f t = coerce $ (coerce (t (Proxy :: Proxy g)) :: a -> a)
            . (coerce (f (Proxy :: Proxy f)) :: a -> a)
{-# INLINE fromTo #-}
infix 2 `fromTo`



(~>) :: forall f g a. Coercible a (g a)
  => Convertor f (From a) -> Convertor g (To a) -> g a -> g a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

(~~>) :: forall f g a.
  Convertor f (From a) -> Convertor g (To a) -> a -> a
f ~~> t = (coerce (t (Proxy :: Proxy g)) :: a -> a)
            . (coerce (f (Proxy :: Proxy f)) :: a -> a)
{-# INLINE (~~>) #-}
infix 2 ~~>





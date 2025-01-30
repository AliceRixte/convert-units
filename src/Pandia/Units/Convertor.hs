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
  convertor _ =
    convertor (Proxy :: Proxy f) -*- convertor (Proxy :: Proxy g)

instance (ConvertorClass f a, Num a, KnownNat n)
  => ConvertorClass ( f -^- n) a where
  convertor _ =
    convertor (Proxy :: Proxy f) -^- fromInteger (natVal (Proxy :: Proxy n))


per :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
per f g _ a = f (Proxy :: Proxy f) a *
              (coerce (g (Proxy :: Proxy g)) :: a -> a) 1
infix 6 `per`

(-/-) :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
(f -/- g) a = per f g a
{-# INLINE (-/-) #-}

(-*-) :: forall a. Num a => (a -> a) -> (a -> a) -> (a -> a)
(f -*- g) a = f a * g a
{-# INLINE (-*-) #-}


(-^-) :: forall a. Num a => (a -> a) -> Int -> (a -> a)
f -^- n | n < 0 = coerce $ (coerce f :: Per a -> Per a) -^- (-n)
        | n == 0 = id
        | n == 1 = f
        | otherwise = f -*- (f -^- (n - 1))
{-# INLINE (-^-) #-}



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





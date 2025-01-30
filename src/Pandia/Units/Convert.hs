{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Pandia.Units.Convert
  ( module Pandia.Units.Convert
  ) where

import Data.Coerce
import Data.Proxy
import Data.Kind
import Data.Functor

import GHC.TypeLits

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


class ConvertType (f :: Type -> Type) a where
  convertType :: Proxy f -> a -> a

instance (ConvertType f a, ConvertType g (Per a), Num a)
  => ConvertType ( f -/- g) a where
  convertType _ =
    convertType (Proxy :: Proxy f) -/- convertType (Proxy :: Proxy g)

instance (ConvertType f a, ConvertType g a, Num a)
  => ConvertType ( f -*- g) a where
  convertType _ =
    convertType (Proxy :: Proxy f) -*- convertType (Proxy :: Proxy g)

instance (ConvertType f a, Num a, KnownNat n)
  => ConvertType ( f -^- n) a where
  convertType _ =
    convertType (Proxy :: Proxy f) -^- fromInteger (natVal (Proxy :: Proxy n))

convert :: forall f g a.
  ( Coercible (f a) a, Coercible a (g a)
  , ConvertType f (From a), ConvertType g (To a))
  => f a -> g a
convert fa = coerce $
  fromTo ( convertType (Proxy :: Proxy f) :: From a -> From a )
         ( convertType (Proxy :: Proxy g) :: To a -> To a )
         ( coerce fa :: a )

per :: forall a. Num a =>  (a -> a) -> (Per a -> Per a)-> (a -> a)
per f g a = f a * (coerce g :: a -> a) 1
infix 6 `per`

times :: forall f g a. Num a => Proxy (f -*- g) -> (Proxy f -> a -> a) -> (Proxy g -> a -> a) -> (a -> a)
times _ f g a = f (Proxy  :: Proxy f) a * g (Proxy :: Proxy g) a

(-/-) :: forall a. Num a => (a -> a) -> (Per a -> Per a) -> (a -> a)
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



fromTo :: forall a. (From a -> From a) -> (To a -> To a) -> a -> a
fromTo f t = (coerce t :: a -> a) . (coerce f :: a -> a)
{-# INLINE fromTo #-}
infix 2 `fromTo`



(~>) :: (From a -> From a) -> (To a -> To a) -> a -> a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>



(<&>~) :: forall f g a. (Functor f, ConvertType g (To (g a)))
  => f (g a)-> (From (g a)->  From (g a)) -> f (g a)
fa <&>~ f = fa <&>
  fromTo f (convertType (Proxy :: Proxy g) :: To (g a) -> To (g a))

(*~) :: forall f  a. (ConvertType f (To a), Coercible a (f a))
  => a -> (From a -> From a) -> f a
a *~ f = coerce $ fromTo f (convertType (Proxy :: Proxy f) :: To a -> To a) a
infix 4 *~





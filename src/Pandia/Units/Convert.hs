module Pandia.Units.Convert
  ( module Pandia.Units.Convert
  ) where

import Data.Coerce
import Data.Proxy
import Data.Kind

newtype From a = From a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

newtype To a = To a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

newtype Per a = Per a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
          , RealFrac, RealFloat, Bounded)

newtype AccType a = AccType a

fromTo :: forall a. (From a -> From a) -> (To a -> To a) -> a -> a
fromTo f t = (coerce t :: a -> a) . (coerce f :: a -> a)
{-# INLINE fromTo #-}
infix 2 `fromTo`

(~>) :: (From a -> From a) -> (To a -> To a) -> a -> a
(~>) = fromTo
{-# INLINE (~>) #-}
infix 2 ~>

per :: forall a. Num a =>  (a -> a) -> (Per a -> Per a)-> (a -> a)
per f g a = f a * (coerce g :: a -> a) 1
infix 7 `per`

(.^) :: (a -> a) -> Int -> (a -> a)
(.^) f n  | n <= 0    = error "Exponent must be positive"
          | n == 1    = f
          | otherwise = f . (f .^ (n - 1))
{-# INLINE (.^) #-}
infixl 9 .^

class ConvertType (f :: Type -> Type) a where
  convertType :: Proxy f -> a -> a





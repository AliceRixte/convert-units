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

import GHC.TypeLits

import Pandia.Units.Rel

-- | A unit is represented by a newtype constructor
type Unit = Type -> Type

-- | A convertor that can convert from and to some unit.
--
-- Convertors can be combined via 'mul'@, @'per'@, and @'pow'@ .
--
type Convertor (f :: Unit) a = Proxy f -> a -> a

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

-- | Forces a convertor to be from SI to its unit
coerceTo :: Convertor f a -> Convertor f (To a)
coerceTo f p = coerce (f p)
{-# INLINE coerceTo #-}

-- | Forces a convertor to be from its unit to SI
coerceFrom :: Convertor f a -> Convertor f (From a)
coerceFrom f p = coerce (f p)
{-# INLINE coerceFrom #-}






-- | Multiplication of two units.
--
-- @
-- type MyForceMoment a = (Newton -*- Meter) a
-- @
--
newtype ((f :: Unit) -*- (g :: Unit)) a = MulDim (f (g a))
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
newtype ((f :: Unit) -/- (g :: Unit)) a = PerDim (f (g a))
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
newtype ((f :: Unit) -^- (n :: Rel)) a = PowDim (f a)
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


-- | Create a convertor from a unit newtype
--
class ConvertorClass (f :: Unit) a where
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

instance (PowClass f n a, ConvertorClass f a, RelVal n)
  => ConvertorClass ( f -^- n) a where
  convertor =
    (convertor :: Convertor f a) -^- fromInteger (relVal (Proxy :: Proxy n))
  {-# INLINE convertor #-}



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

-- Nested convertor division is not supported.
--
-- @
-- >>> (kilo meter -/- (second -/- kilo newton)  ~~> meter -/- second ) (5 :: Float)
--
-- <interactive>:73:29: error: [GHC-39999] • No instance for ‘KiloClass (Per
--     (Per (From Float)))’
--  @
--
-- This feature could be added by adding @('Per' ('Per' a))@ for every instances
-- of  @'ConvertorClass'@. If you want to contribute, this is fairly simple to
-- do although very repetitive.
--
per :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
per f g _ a = f (Proxy :: Proxy f) a *
              (coerce (g (Proxy :: Proxy g)) :: a -> a) 1
infix 6 `per`

-- | Infix synonym for @'per'@
--
-- @
-- >>> (kilo meter -/- hour ~~> meter -/- second ) (5 :: Float) 1.388889 @
-- @
--
(-/-) :: forall f g a. Num a
  =>  Convertor f a -> Convertor g (Per a) -> Convertor (f -/- g) a
(f -/- g) a = per f g a
{-# INLINE (-/-) #-}

invert :: forall f a. Convertor f a -> Convertor f (Per a)
invert f _ = coerce (f (Proxy :: Proxy f))


-- | Multiplication of convertors. / Warning  : Use with caution \
--
-- @
-- >>> (newton `mul` kilo meter  ~~> newton `mul` meter ) (5 :: Float) 5000 @
--
-- In the previous case it works fine, and as long as you only use international
-- system units it will work too. However, if you want to use conversions that
-- have an offset, like Celsius -> Kelvin, there is a problem.
--
-- The good news is it rarely (I didn't find a real life example yet) happens in
-- concrete real life examples.
--
-- [What is the problem ?]
--
-- To be able to compute the multiplication of two convertors, we rely on the
-- -- **false** assumption that the composition of two convertors is
-- commutative.
--
-- This is true when all convertors are just a multiplication factor, for
-- instance when converting between kilometers and meters, or between kilometers
-- and miles.
--
-- The good news is that in most of the cases that makes sense it is the case.
--
-- However, everything falls apart as soon as some convertors use an offset,
-- because as soon as we combine them with multiplicators we do not have
-- commutativity any more.
--
-- Here is a concrete example :
--
-- @
-- >>> kilo meter  -*- celsius ~~> meter -*- kelvin )  1 274150.0 celsius -*-
-- kilo meter ~~> kelvin -*- meter )  1 1273.15 @
--
-- [When to trust convertor multiplication ?]
--
-- 1. when using only use standard units and prefixes, as every conversion is
-- just a multiplication  corresponding to the prefix
-- 2. when using only convertors that are just a multiplying factor
--
-- There might be other cases where it works but in this case you should know
-- what you are doing.
--
-- [Is this fixable ?]
--
-- Probably not. There is a possibility to add a @Mul@ newtype just like for
-- @'Per'@ to be able to know whenever we are in a multiplication, but I don't
-- see how this information would help, except for a few fringe cases that
-- probably are not worth the investment.
--
-- For now, it will stay like this
--
mul :: forall f g a. Num a
  => Convertor f a -> Convertor g a -> Convertor (f -*- g) a
mul f g _ = f (Proxy :: Proxy f) . g (Proxy :: Proxy g)
{-# INLINE mul #-}
infixl 7 `mul`


(-*-) :: forall f g a. Num a =>
  Convertor f a -> Convertor g a -> Convertor (f -*- g) a
(-*-) =mul
{-# INLINE (-*-) #-}


timesFun  :: Int -> (a -> a) -> a -> a
timesFun 0 _ = id
timesFun n f = f . timesFun (n - 1) f
{-# INLINE timesFun #-}

powConv :: Convertor f a -> Int -> a -> a
powConv f n | n >= 0  = timesFun n (f (Proxy :: Proxy f))
            | n < 0   = coerce $ timesFun n (invert f (Proxy :: Proxy f))
{-# INLINE powConv #-}

-- powNeg :: Convertor f (Per a) -> Int -> a -> a
-- powNeg f 0 = id
-- powNeg f 1 = coerce $ f (Proxy :: Proxy f)
-- powNeg f n = coerce $ f (Proxy :: Proxy f) . powNeg f (n - 1)
-- {-# INLINE powNeg #-}

class PowClass (f :: Unit) (n::Rel) a where
  pow :: Convertor f a -> Int -> Convertor (f -^- n) a
  infix 8 `pow`

instance PowClass f (Pos 0) a where
  pow _ 0 _ = coerce (id :: a -> a)
  pow _ _ _ = error "The exponent doesn't match the dimension"
  {-# INLINE pow #-}


-- instance (PowClass f (Pos n) a, np1 ~ n + 1, Num a)
--   => PowClass f (Pos np1) a where
--   pow f n _ a = f (Proxy :: Proxy f) a
--                 * pow f (n - 1) (Proxy :: Proxy (f -^- Pos n)) a
--   {-# INLINE pow #-}

-- instance PowClass f (Neg 0) a where
--   pow _ 0 _ = coerce (id :: a -> a)
--   pow _ _ _ = error "The exponent doesn't match the dimension"
--   {-# INLINE pow #-}

-- instance PowClass f (Neg 1) a where
--   pow f (-1) _ a = coerce (f (Proxy :: Proxy f) (Per a))


(-^-) :: forall f n a.  PowClass f n a
  => Convertor f a -> Int -> Convertor (f -^- n) a
(-^-) = pow


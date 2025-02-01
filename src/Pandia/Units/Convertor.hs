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
  , Not
  ) where

import Data.Coerce
import Data.Proxy
import Data.Kind
import Data.Type.Bool

import Pandia.Units.Rel

----------------------------- Unit construction ------------------------------

-- | A unit is represented by a newtype constructor. A quantity of some newtype
-- @f@ is of type @f a@.
type Unit = Type -> Type


-- | A unit that has no dimension.
--
-- @
-- type MyHertz = NoUnit -/- Second
-- @
--
newtype NoUnit a = NoUnit a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Bounded, Enum, Semigroup, Monoid, Functor)


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



type family ElimDiv (u :: Unit) :: Unit where
    ElimDiv (u -*- v) = ElimDiv u -*- ElimDiv v
    ElimDiv (u -^- n) = ElimDiv u -^- n
    ElimDiv (u -/- u) = NoUnit
    ElimDiv (u -/- v) = ElimDiv u -*- ElimDiv v -^- Neg 1
    ElimDiv u = u

type family ElimPow (u :: Unit) :: Unit where
  ElimPow (u -/- v) = ElimPow u -/- ElimPow v
  ElimPow (u -*- v) = ElimPow u -*- ElimPow v
  ElimPow (u -^- Pos 0) = NoUnit
  ElimPow (u -^- Neg 0) = NoUnit
  ElimPow (u -^- Pos 1) = ElimPow u
  ElimPow ((u -^- n) -^- m) = ElimPow (u -^- (n `MulRel` m))
  ElimPow ((u -*- v) -^- n) = ElimPow (u -^- n -*- v -^- n)
  ElimPow u = u

type family ElimNoUnit (u :: Unit) :: Unit where
  ElimNoUnit (NoUnit -*- u) = ElimNoUnit u
  ElimNoUnit (u -*- NoUnit) = ElimNoUnit u
  ElimNoUnit (u -*- v) = ElimNoUnit u -*- ElimNoUnit v
  ElimNoUnit u = u

type family SimplifyUnit (u :: Unit) :: Unit where
  SimplifyUnit u = ElimNoUnit (ElimPow (ElimDiv u))


--------------------------------- Conversion ---------------------------------

data ConversionDirection  = FromSI | ToSI

type IsPer = Bool
type Per = 'True

data ConversionInfo = ConversionInfo Unit ConversionDirection IsPer



-- | A convertor that can convert from and to some unit.
--
-- Convertors can be combined via 'mul'@, @'per'@, and @'pow'@ .
--
type Convertor (u :: Unit) (cd :: ConversionDirection) (p :: IsPer) a
  = Proxy ('ConversionInfo u cd p) -> a -> a

runConvertor :: Convertor u cd p a -> a -> a
runConvertor f = f Proxy
{-# INLINE runConvertor #-}

coerceConvertor :: Convertor u cd p a -> Convertor v cd' p' a
coerceConvertor f _ = f Proxy
{-# INLINE coerceConvertor #-}

coerceTo :: Convertor u cd p a -> Convertor u 'ToSI 'False a
coerceTo f _ = f Proxy
{-# INLINE coerceTo #-}

coerceFrom :: Convertor u cd p a -> Convertor u 'FromSI 'False a
coerceFrom f _ = f Proxy
{-# INLINE coerceFrom #-}

-- fromSI' :: Convertor u 'FromSI (Not Per) -> a -> a
-- fromSI' f = f (Proxy :: Proxy ('ConversionInfo u 'FromSI (Not Per)))
-- {-# INLINE fromSI' #-}


-- -- | When a quantity decorated by this newtype is fed to a convertor, the
-- -- convertor will compute the conversion from its unit to the international
-- -- system unit
-- newtype ToSI a = ToSI a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
--           , RealFrac, RealFloat, Bounded)

-- -- | When a quantity decorated by this newtype is fed to a convertor, the
-- -- convertor will compute the conversion from the international system unit to
-- -- its unit
-- newtype FromSI a = FromSI a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
--           , RealFrac, RealFloat, Bounded)

-- -- | When receiving a quantity of the form @'Per' ('ToSI' a)@, the convertor
-- -- will compute the conversion from the international system unit to its
-- -- inverted unit
-- --
-- -- Similarly, when receiving a quantity of the form @'Per' ('FromSI' a)@, the
-- -- convertor will compute the conversion from its unit to the international
-- -- system unit
-- --
-- newtype Per a = Per a
--   deriving (Show, Eq, Ord, Num, Fractional, Floating, Real
--           , RealFrac, RealFloat, Bounded)








-- | Create a convertor from a unit newtype
--
class ConvertorClass (u :: Unit) (cd :: ConversionDirection) (p :: IsPer) a
  where
  convertor :: Convertor u cd p a
  convertor _ = id
  {-# INLINE convertor #-}

instance (ConvertorClass u cd p a, ConvertorClass v cd (Not p) a, Num a)
  => ConvertorClass (u -/- v) cd p a where
  convertor = (convertor :: Convertor u cd p a)
          -/- (convertor :: Convertor v cd (Not p) a)
  {-# INLINE convertor #-}

instance (ConvertorClass u cd p a, ConvertorClass v cd p a, Num a)
  => ConvertorClass (u -*- v) cd p a where
  convertor =
    (convertor :: Convertor u cd p a) -*- (convertor :: Convertor v cd p a)
  {-# INLINE convertor #-}

instance (ConvertorClass u cd p a, KnownRel n)
  => ConvertorClass (u -^- n) cd p a where
  convertor =
    (convertor :: Convertor u cd p a) -^- fromInteger (relVal (Proxy :: Proxy n))
  {-# INLINE convertor #-}






-- | A convertor that does nothing
--
-- @
-- >>> (nounit `per` milli seconds ~~> hertz) (5 :: Int)
-- 5000
-- @
--
nounit :: Convertor NoUnit cd p a
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
--     (Per (ToSI Float)))’
--  @
--
-- This feature could be added by adding @('Per' ('Per' a))@ for every instances
-- of  @'ConvertorClass'@. If you want to contribute, this is fairly simple to
-- do although very repetitive.
--
per :: forall u v cd p a. Num a =>
  Convertor u cd p a -> Convertor v cd (Not p) a -> Convertor (u -/- v) cd p a
per f g _ a = runConvertor f a
            * runConvertor (coerceConvertor g :: Convertor v cd (Not p) a) 1
infix 6 `per`

-- | Infix synonym for @'per'@
--
-- @
-- >>> (kilo meter -/- hour ~~> meter -/- second ) (5 :: Float) 1.388889 @
-- @
--
(-/-) :: forall u v cd p a. Num a =>
  Convertor u cd p a -> Convertor v cd (Not p) a -> Convertor (u -/- v) cd p a
(f -/- g) a = per f g a
{-# INLINE (-/-) #-}



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
mul :: forall u v cd p a. Num a
  => Convertor u cd p a -> Convertor v cd p a -> Convertor (u -*- v) cd p a
mul f g _ = runConvertor f . runConvertor g
{-# INLINE mul #-}
infixl 7 `mul`


(-*-) :: forall u v cd p a. Num a
  => Convertor u cd p a -> Convertor v cd p a -> Convertor (u -*- v) cd p a
(-*-) =mul
{-# INLINE (-*-) #-}


timesFun  :: Int -> (a -> a) -> a -> a
timesFun 0 _ = id
timesFun n f = f . timesFun (n - 1) f
{-# INLINE timesFun #-}

powConv :: forall u cd p a. Convertor u cd p a -> Int -> a -> a
powConv f n | n >= 0  = timesFun n (runConvertor f)
            | n < 0   = timesFun n
              (runConvertor (coerceConvertor f :: Convertor u cd (Not p) a))
{-# INLINE powConv #-}



pow :: forall u cd p n a. KnownRel n
  => Convertor u cd p a -> Int -> Convertor (u -^- n) cd p a
pow f n _ = if n == fromInteger (relVal (Proxy :: Proxy n)) then
            coerce $ powConv f n
          else
            error "The exponent doesn't match the dimension"
{-# INLINE pow #-}
infix 8 `pow`


-- instance PowClass f (Pos 0) a where
--   pow _ 0 _ = coerce (id :: a -> a)
--   pow _ _ _ = error "The exponent doesn't match the dimension"
--   {-# INLINE pow #-}


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


(-^-) :: KnownRel n => Convertor u cd p a -> Int -> Convertor (u -^- n) cd p a
(-^-) = pow


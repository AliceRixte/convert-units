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
import GHC.TypeLits

import Pandia.Units.Rel
import Pandia.Units.Unit



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
runConvertor u = u Proxy
{-# INLINE runConvertor #-}

coerceConvertor :: Convertor u cd p a -> Convertor v cd' p' a
coerceConvertor u _ = u Proxy
{-# INLINE coerceConvertor #-}

coerceTo :: Convertor u cd p a -> Convertor u 'ToSI 'False a
coerceTo u _ = u Proxy
{-# INLINE coerceTo #-}

coerceFrom :: Convertor u cd p a -> Convertor u 'FromSI 'False a
coerceFrom u _ = u Proxy
{-# INLINE coerceFrom #-}


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



-- instance (ConvertorClass u cd p a)
--   => ConvertorClass (u -^- n) cd p a where
--   convertor _ =
--     powConv (convertor :: Convertor u cd p a)  2
--   {-# INLINE convertor #-}

instance (ConvertorClass u cd p a, KnownRel n)
  => ConvertorClass (u -^- n) cd p a where
  convertor =
    pow (convertor :: Convertor u cd p a)  (Proxy :: Proxy n)
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
per u g _ a = runConvertor u a
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
(u -/- g) a = per u g a
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
mul u g _ = runConvertor u . runConvertor g
{-# INLINE mul #-}
infixl 7 `mul`


(-*-) :: forall u v cd p a. Num a
  => Convertor u cd p a -> Convertor v cd p a -> Convertor (u -*- v) cd p a
(-*-) =mul
{-# INLINE (-*-) #-}


timesFun  :: Int -> (a -> a) -> a -> a
timesFun 0 _ = id
timesFun n u = u . timesFun (n - 1) u
{-# INLINE timesFun #-}

powConv :: forall u cd p a. Convertor u cd p a -> Int -> a -> a
powConv u n | n >= 0  = timesFun n (runConvertor u)
            | n < 0   = timesFun n
              (runConvertor (coerceConvertor u :: Convertor u cd (Not p) a))
{-# INLINE powConv #-}


p0 :: Proxy (Pos 0)
p0 = Proxy

p1 :: Proxy (Pos 1)
p1 = Proxy

p2 :: Proxy (Pos 2)
p2 = Proxy

p3 :: Proxy (Pos 3)
p3 = Proxy

p4 :: Proxy (Pos 4)
p4 = Proxy

m1 :: Proxy (Neg 1)
m1 = Proxy

m2 :: Proxy (Neg 2)
m2 = Proxy

m3 :: Proxy (Neg 3)
m3 = Proxy

m4 :: Proxy (Neg 4)
m4 = Proxy


pow :: forall u cd p n a. KnownRel n
   => Convertor u cd p a -> Proxy n -> Convertor (u -^- n) cd p a
pow f _ _ = powConv f $ fromInteger (relVal (Proxy :: Proxy n))
{-# INLINE pow #-}
infix 8 `pow`
-- pow :: forall u cd p a.
--   Convertor u cd p a -> Int -> Convertor (u -^- Pos 0) cd p a

-- pow :: forall u cd p a.
--   => Convertor u cd p a -> Int -> Convertor (u -^- Pos 0) cd p a

-- newtype Blub = Blub Rel

-- class PowClass u cd p (n :: Rel) a where
--   pow :: Convertor u cd p a -> Int -> Convertor (u -^- n) cd p a

--   infix 8 `pow`

-- instance PowClass u cd p (Pos 0) a where
--   pow _ 0 _ = coerce (id :: a -> a)
--   pow _ _ _ = error "The exponent doesn't match the dimension"
--   {-# INLINE pow #-}


-- instance (PowClass u cd p (Pos n) a, np1 ~ n + 1, Num a)
--   => PowClass u cd p (Pos np1) a where
--   pow u n _  = runConvertor u . runConvertor
--           ((pow ::  Convertor u cd p a -> Int -> Convertor (u -^- Pos n) cd p a)
--           (coerceConvertor u)  (n - 1))
--                 -- pow u (n - 1) (Proxy :: Proxy (u -^- Pos n)) a
--   {-# INLINE pow #-}

-- instance PowClass u (Neg 0) a where
--   pow _ 0 _ = coerce (id :: a -> a)
--   pow _ _ _ = error "The exponent doesn't match the dimension"
--   {-# INLINE pow #-}

-- instance PowClass u (Neg 1) a where
--   pow u (-1) _ a = coerce (u (Proxy :: Proxy u) (Per a))


(-^-) :: KnownRel n =>
   Convertor u cd p a -> Proxy n -> Convertor (u -^- n) cd p a
(-^-) = pow


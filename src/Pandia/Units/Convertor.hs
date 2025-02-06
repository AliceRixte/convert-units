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
  --  Convertor
  -- , FromSys, ToSys
  -- , fromSys'
  -- , nounit, (-*-), (-/-), (-^-)
  -- , mul, per, pow
  -- , p0, p1, p2, p3, p4, m1, m2, m3, m4
  -- , ConvertorClass (..)
  -- , ConversionDirection (..)
  -- , runConvertor
  -- , coerceFrom, coerceTo
  ) where

import Data.Coerce
import Data.Proxy

import Data.Type.Bool

import Pandia.Units.Rel
import Pandia.Units.Unit
import Pandia.Units.Dimension



--------------------------------- Conversion ---------------------------------

data ConversionDirection  = FromDimSys | ToDimSys

type IsMul = Bool
-- type Per = 'True

data ConversionInfo = ConversionInfo Unit ConversionDirection IsMul


-- | A convertor that can convert from and to some unit.
--
-- Convertors can be combined via 'mul'@, @'per'@, and @'pow'@ .
--
type Convertor (u :: Unit) (cd :: ConversionDirection) (p :: IsMul) a
  = Proxy ('ConversionInfo u cd p) -> (a -> a) -> a -> a


type FromSys u a = Convertor u 'FromDimSys 'False a
type ToSys u a = Convertor u 'ToDimSys 'False a


type family UnitToSys (u :: Unit) (sys :: DimSystem k) :: Unit where
  UnitToSys u sys = SimplifyUnit (DimToBaseUnit (DimOf sys u))

runConvertor :: Convertor u cd p a -> a -> a
runConvertor u = u Proxy id
{-# INLINE runConvertor #-}

coerceConvertor :: Convertor u cd p a -> Convertor v cd' p' a
coerceConvertor u _ = u Proxy
{-# INLINE coerceConvertor #-}

unitMultiplier :: forall u cd p a. Num a => Convertor u cd p a -> a
unitMultiplier u = runConvertor (coerceConvertor u :: Convertor u cd True a) 1
{-# INLINE unitMultiplier #-}

fromSys' ::FromSys u a -> a -> a
fromSys' = runConvertor
{-# INLINE fromSys' #-}

toSys' :: ToSys u a -> a -> a
toSys' = runConvertor
{-# INLINE toSys' #-}

convertNoCheck' :: forall u v a.
  ToSys u a -> FromSys v a  -> a -> a
convertNoCheck' u v  = runConvertor v . runConvertor u
{-# INLINE convertNoCheck' #-}

convertNoCheck :: forall u v a. (Coercible a (v a), Coercible a (u a))
   => ToSys u a -> FromSys v a  -> u a -> v a
convertNoCheck u v a = coerce
   $ runConvertor v
   $ runConvertor u
   $ (coerce a :: a)
{-# INLINE convertNoCheck #-}


convertUnits :: forall (u :: Unit) (v :: Unit) sys a.
  ( Coercible a (v a), Coercible a (u a)
  , ConvertorClass u 'ToDimSys 'False a, ConvertorClass v 'FromDimSys 'False a
  , SameDim sys u v
  )
  => Proxy sys -> u a -> v a
convertUnits _ =
    convertNoCheck (convertor :: ToSys u a) (convertor :: FromSys v a)
{-# INLINE convertUnits #-}

asSys :: forall sys u v a.
  (Coercible a (u a), Coercible a (v a)
  , ConvertorClass u 'ToDimSys 'False a
  , SameDim sys u v
  )
  => Proxy sys -> u a -> FromSys v a  -> v a
asSys _ fa v = convertNoCheck (convertor :: ToSys u a ) v fa
{-# INLINE asSys #-}



asNoCheck :: forall u v a.
  (Coercible a (u a), Coercible a (v a), ConvertorClass u 'ToDimSys 'False a)
  => u a -> FromSys v a  -> v a
asNoCheck fa v = convertNoCheck (convertor :: ToSys u a ) v fa
{-# INLINE asNoCheck #-}

convertSys' :: forall sys u v a.
  (Coercible a (v a), Coercible a (u a), SameDim sys u v)
  => Proxy sys -> ToSys u a -> FromSys v a -> a -> a
convertSys' _ = convertNoCheck'

fromSys :: forall u a. (Coercible a (u a))
  => FromSys u a -> a -> u a
fromSys u = coerce (runConvertor u)
{-# INLINE fromSys #-}

toSys :: forall sys u a. Coercible a ((UnitToSys u sys) a)
  => Proxy sys -> ToSys u a -> a -> (UnitToSys u sys) a
toSys _ u = coerce (runConvertor u)
{-# INLINE toSys #-}









-- flipFromTo :: FromSys u a -> ToSys u a
-- flipFromTo = coerceConvertor
-- {-# INLINE flipFromTo #-}

coerceTo :: Convertor u cd p a -> ToSys u a
coerceTo u _ = u Proxy
{-# INLINE coerceTo #-}

coerceFrom :: Convertor u cd p a -> FromSys u a
coerceFrom u _ = u Proxy
{-# INLINE coerceFrom #-}



-- | Create a convertor from a unit newtype
--
class ConvertorClass (u :: Unit) (cd :: ConversionDirection) (p :: IsMul) a
  where
  convertor :: Convertor u cd p a
  convertor _ _ = id
  {-# INLINE convertor #-}

instance (ConvertorClass u cd True a, ConvertorClass v cd True a
        , PerClass p a)
  => ConvertorClass (u -/- v) cd p a where
  convertor = (convertor :: Convertor u cd True a)
          -/- (convertor :: Convertor v cd True a)
  {-# INLINE convertor #-}

instance (ConvertorClass u cd True a, ConvertorClass v cd True a, MulClass p a)
  => ConvertorClass (u -*- v) cd p a where
  convertor = (convertor :: Convertor u cd True a)
          -*- (convertor :: Convertor v cd True a)
  {-# INLINE convertor #-}

instance (ConvertorClass u cd True a, PowClass p a, KnownRel n)
  => ConvertorClass (u -^- n) cd p a where
  convertor =
    pow (convertor :: Convertor u cd True a) (Proxy :: Proxy n)
  {-# INLINE convertor #-}




-- | A convertor that does nothing
--
-- @
-- >>> (nounit `per` milli seconds ~~> hertz) (5 :: Int)
-- 5000
-- @
--
nounit :: Convertor NoUnit cd p a
nounit _ _ = id
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
class Fractional a => PerClass p a where
  per ::
    Convertor u cd True a -> Convertor v cd True a -> Convertor (u -/- v) cd p a
  infix 6 `per`

instance Fractional a => PerClass True a where
  per u v _ _ _ = unitMultiplier u / unitMultiplier v
  {-# INLINE per #-}

instance Fractional a =>  PerClass False a where
  per u v _ _ a = a * unitMultiplier u / unitMultiplier v
  {-# INLINE per #-}

-- per :: forall u v cd p a. Fractional a =>


-- | Infix synonym for @'per'@
--
-- @
-- >>> (kilo meter -/- hour ~~> meter -/- second ) (5 :: Float) 1.388889 @
-- @
--
(-/-) :: forall u v cd p a. PerClass p a  =>
  Convertor u cd True a -> Convertor v cd True a -> Convertor (u -/- v) cd p a
(u -/- v) a = per u v a
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
class Num a => MulClass p a where
  mul ::
    Convertor u cd True a -> Convertor v cd True a -> Convertor (u -*- v) cd p a
  infix 7 `mul`

instance Num a => MulClass True a where
  mul u v _ _ _ = unitMultiplier u * unitMultiplier v
  {-# INLINE mul #-}

instance Num a => MulClass False a where
  mul u v _ _ a = a * unitMultiplier u * unitMultiplier v
  {-# INLINE mul #-}

-- mul :: forall u v cd p a. Num a
--   => Convertor u cd True a -> Convertor v cd True a
--   -> Convertor (u -*- v) cd p a
-- mul u v _ = runConvertor u . runConvertor v
-- {-# INLINE mul #-}
-- infixl 7 `mul`


(-*-) :: forall u v cd p a. MulClass p a
  => Convertor u cd True a -> Convertor v cd True a
  -> Convertor (u -*- v) cd p a
(-*-) = mul
{-# INLINE (-*-) #-}


-- timesFun  :: Int -> (a -> a) -> a -> a
-- timesFun 0 _ = id
-- timesFun n u = u . timesFun (n - 1) u
-- {-# INLINE timesFun #-}

-- powConv :: forall u cd a. Convertor u cd True a -> Int -> a -> a
-- powConv u n | n >= 0     = timesFun n (runConvertor u)
--             | otherwise  = timesFun n
--               (runConvertor (coerceConvertor u :: Convertor u cd  a))
-- {-# INLINE powConv #-}


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

class Fractional a => PowClass p a where
  pow :: KnownRel n =>
    Convertor u cd True a -> Proxy n -> Convertor (u -^- n) cd p a
  infix 8 `pow`

instance Fractional a => PowClass True a where
  pow u n _ _ _ = unitMultiplier u ^^ fromInteger (relVal n)
  {-# INLINE pow #-}

instance Fractional a => PowClass False a where
  pow u n _ _ a = a * unitMultiplier u ^^ fromInteger (relVal n)
  {-# INLINE pow #-}




-- pow :: forall u cd p n a. KnownRel n
--    => Convertor u cd p a -> Proxy n -> Convertor (u -^- n) cd p a
-- pow f _ _ = powConv f $ fromInteger (relVal (Proxy :: Proxy n))
-- {-# INLINE pow #-}
-- infix 8 `pow`


(-^-) :: (PowClass p a, KnownRel n) =>
   Convertor u cd True a -> Proxy n -> Convertor (u -^- n) cd p a
(-^-) = pow


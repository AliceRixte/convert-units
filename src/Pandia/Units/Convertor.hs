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
  -- , nounit, (~*), (~/), (~^)
  -- , mul, per, pow
  -- , p0, p1, p2, p3, p4, m1, m2, m3, m4
  -- , ConvertorClass (..)
  -- , ConversionDirection (..)
  -- , runConvertor
  -- , coerceFrom, coerceTo
  ) where

import Data.Coerce
import Data.Proxy


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
  = Proxy ('ConversionInfo u cd p) -> a -> a


type FromSys u a = Convertor u 'FromDimSys 'False a
type ToSys u a = Convertor u 'ToDimSys 'False a


type family UnitToSys (u :: Unit) (sys :: DimSystem k) :: Unit where
  UnitToSys u sys = SimplifyUnit (DimToBaseUnit (DimOf sys u))

runConvertor :: Convertor u cd p a -> a -> a
runConvertor u = u Proxy
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
  convertor _  = id
  {-# INLINE convertor #-}

instance (ConvertorClass u cd True a, ConvertorClass v cd True a
        , PerClass p a)
  => ConvertorClass (u -/- v) cd p a where
  convertor = (convertor :: Convertor u cd True a)
          ~/ (convertor :: Convertor v cd True a)
  {-# INLINE convertor #-}

instance (ConvertorClass u cd True a, ConvertorClass v cd True a, MulClass p a)
  => ConvertorClass (u -*- v) cd p a where
  convertor = (convertor :: Convertor u cd True a)
          ~* (convertor :: Convertor v cd True a)
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
nounit _ = id
{-# INLINE nounit #-}





class Fractional a => PerClass p a where
  -- | Division of convertors
  --
  -- @
  -- >>> (kilo meter `per` hour ~~> meter `per` second ) (5 :: Float)
  -- 1.388889
  -- @
  per ::
    Convertor u cd True a -> Convertor v cd True a -> Convertor (u -/- v) cd p a
  infix 6 `per`

instance Fractional a => PerClass True a where
  per u v _ _ = unitMultiplier u / unitMultiplier v
  {-# INLINE per #-}

instance Fractional a =>  PerClass False a where
  per u v _ a = a * unitMultiplier u / unitMultiplier v
  {-# INLINE per #-}



-- | Infix synonym for @'per'@
--
-- @
-- >>> (kilo meter ~/ hour ~~> meter ~/ second ) (5 :: Float) 1.388889 @
-- @
--
(~/) :: forall u v cd p a. PerClass p a  =>
  Convertor u cd True a -> Convertor v cd True a -> Convertor (u -/- v) cd p a
(u ~/ v) a = per u v a
{-# INLINE (~/) #-}
infix 6 ~/




class Num a => MulClass p a where
  -- | Multiplication of convertors.
  --
  -- @
  -- >>> ( meter ~* gram ~* second ~^ m2  ~~> newton) 5
  -- 5.0e-3
  -- @
  --
  mul ::
    Convertor u cd True a -> Convertor v cd True a -> Convertor (u -*- v) cd p a
  infix 7 `mul`

instance Num a => MulClass True a where
  mul u v _ _ = unitMultiplier u * unitMultiplier v
  {-# INLINE mul #-}

instance Num a => MulClass False a where
  mul u v _ a = a * unitMultiplier u * unitMultiplier v
  {-# INLINE mul #-}


(~*) :: forall u v cd p a. MulClass p a
  => Convertor u cd True a -> Convertor v cd True a
  -> Convertor (u -*- v) cd p a
(~*) = mul
{-# INLINE (~*) #-}
infixl 7 ~*




class Fractional a => PowClass p a where
  pow :: KnownRel n =>
    Convertor u cd True a -> Proxy n -> Convertor (u -^- n) cd p a
  infix 8 `pow`

instance Fractional a => PowClass True a where
  pow u n _ _ = unitMultiplier u ^^ (fromInteger (relVal n) :: Int)
  {-# INLINE pow #-}

instance Fractional a => PowClass False a where
  pow u n _ a = a * unitMultiplier u ^^ (fromInteger (relVal n) :: Int)
  {-# INLINE pow #-}



(~^) :: (PowClass p a, KnownRel n) =>
   Convertor u cd True a -> Proxy n -> Convertor (u -^- n) cd p a
(~^) = pow
infix 8 ~^


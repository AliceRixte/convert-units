module Pandia.Units.NonStd.Frequency
  ( module Pandia.Units.NonStd.Frequency
  ) where

import GHC.TypeLits
import Data.Proxy

import Pandia.Units.Core

import Data.Monoid

import Data.Shiftable
import Data.Act

newtype Tet (n :: Nat) (s :: Rel) a = Tet a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

deriving via (ActSelf' (Product a)) instance Num a =>
  LAct (Tet b offs a) (Product a)


instance Num a => Origin (Tet n s a) where
  origin = 0
  {-# INLINE origin #-}

instance Num a => RActCyclic (Tet n offs a) (Last (Tet n offs a)) where
  rorigin' = 0
  rshift = Last . Just

instance Num a => LActCyclic (Tet n offs a) (First (Tet n offs a)) where
  lorigin' = 0
  lshift = First . Just


instance HasDim syst (Tet n s) where
  type DimOf syst (Tet n s) = DimT syst (Neg 1)

-- | Convertor for Tone Equal Temperament
--
tet :: (KnownNat n, KnownRel s)
  => ConvertorClass (Tet n s) cd p a => Proxy n -> Proxy s ->
      Convertor (Tet n s) cd p a
tet _ _ = convertor
{-# INLINE tet #-}

instance (KnownNat n, KnownRel s, Floating a)
  => ConvertorClass (Tet n s) 'ToDimSys 'False a where
  convertor _ x = let n = fromIntegral $ natVal (Proxy :: Proxy n) :: a
                      s = fromIntegral $ relVal (Proxy :: Proxy s) :: a
                  in
                    440 * 2 ** ((x + s / 100) / n)
  {-# INLINE convertor #-}

instance (KnownNat n, KnownRel s, Floating a)
  => ConvertorClass (Tet n s) 'FromDimSys 'False a where
  convertor _ x = let n = fromIntegral $ natVal (Proxy :: Proxy n) :: a
                      s = fromIntegral $ relVal (Proxy :: Proxy s) :: a
                  in
                    n * logBase 2 (x / 440) - s / 100
  {-# INLINE convertor #-}


type MidiPitch = Tet 12 (Neg 6900)

midiPitch :: ConvertorClass MidiPitch cd p a => Convertor MidiPitch cd p a
midiPitch = tet (Proxy :: Proxy 12) (Proxy :: Proxy (Neg 6900))
{-# INLINE midiPitch #-}



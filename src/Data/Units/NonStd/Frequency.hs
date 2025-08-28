module Data.Units.NonStd.Frequency where

import Control.Exception
import Data.Word
import Data.Fixed
import GHC.TypeLits
import Data.Proxy

import Data.Units.Base
import Data.Units.SI.Units

-- | Pitch in the MIDI norm.
--
-- newtype MidiPitch a = MidiPitch a
--   deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
--            , RealFrac, RealFloat, Functor)

-- instance Floating a => From MidiPitch a where
--   from (MidiPitch a) = quantity $ 440 * 2 ** ((a - 69) / 12)

-- instance Floating a => To MidiPitch a where
--   to a = MidiPitch $ 12 * logBase 2 (unQuantity a/ 440) + 69

-- instance IsUnit MidiPitch where
--   type DimOf MidiPitch = Time .^- 1


-- | Frequency in Tone Equal Temperament
--
newtype Tet (b :: Nat) (offs :: ZZ) a = Tet a
  deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)

instance (Floating a, KnownNat b, KnownInt offs) => From (Tet b offs) a where
  from (Tet a) = quantity $ 440 * 2 ** ((a + offs) / b)
    where
     b = fromIntegral $ natVal (Proxy :: Proxy b)
     offs = fromIntegral (intVal (Proxy :: Proxy offs)) / 100

instance (Floating a, KnownNat b, KnownInt offs) => To (Tet b offs) a where
  to a = Tet $ b * logBase 2 (unQuantity a/ 440) - offs
    where
     b = fromIntegral $ natVal (Proxy :: Proxy b)
     offs = fromIntegral (intVal (Proxy :: Proxy offs)) / 100

instance IsUnit (Tet b offs) where
  type DimOf (Tet b offs) = Time .^- 1

instance (KnownNat b, KnownInt offs) => ShowUnit (Tet b offs) where
  type ShowUnitType (Tet b offs) =
    Text "tet{b=" :<>: ShowType b
    :<>: Text ",offs=" :<>: ShowType offs :<>: Text "}"
  showsUnitPrec d = showParen (d > 10) $
    showString "Tet " . shows (natVal (Proxy :: Proxy b))
                     . showString " "
                     . shows (intVal (Proxy :: Proxy offs))
  prettyUnit = "tet{b=" ++ show (natVal (Proxy :: Proxy b))
            ++ ",offs=" ++ show (intVal (Proxy :: Proxy offs)) ++ "}"


type MidiPitch = Tet 12 (Neg 6900)

data PitchException = OutOfMidiRange

instance Exception PitchException

instance Show PitchException where
  show OutOfMidiRange = "A linear pitch is either negative or higher than 127,\
  \ and therefore cannot be converted to MIDI"

safeDecomposePitchCents :: Real a => Tet b offs a -> Maybe (Word8, a)
safeDecomposePitchCents n =
  if n >= 128 || n < 0 then
    Nothing
  else
    Just (divMod' (unQuantity n) 1)

decomposePitchCents :: Real a => Tet b offs a-> (Word8, a)
decomposePitchCents n =
  if n >= 128 || n < 0 then
    throw OutOfMidiRange
  else
    divMod' (unQuantity n) 1


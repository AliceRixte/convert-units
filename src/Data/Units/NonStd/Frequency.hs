module Data.Units.NonStd.Frequency where

import GHC.TypeLits
import Data.Proxy

import Data.Units.Base
import Data.Units.SI.Units


-- | Frequency in Tone Equal Temperament
--
newtype Tet (b :: Nat) (offs :: ZZ) a = Tet a
  deriving ( Eq, Ord, Num, Fractional, Floating, Real
           , RealFrac, RealFloat, Functor)
  deriving Show via MetaUnit (Tet b offs) a

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
  type StdUnitOf (Tet b offs) = Second -^~ 1

instance (KnownNat b, KnownInt offs) => ShowUnit (Tet b offs) where
  type ShowUnitType (Tet b offs) =
    Text "tet{b=" :<>: ShowType b
    :<>: Text ",offs=" :<>: ShowType offs :<>: Text "}"
  showsUnitPrec d = showParen (d > 10) $
    showString "Tet" . shows (natVal (Proxy :: Proxy b))
                     . shows (intVal (Proxy :: Proxy offs))
  prettyUnit = "tet{b=" ++ show (natVal (Proxy :: Proxy b))
            ++ ",offs=" ++ show (intVal (Proxy :: Proxy offs)) ++ "}"

type MidiPitch = Tet 12 (Neg 6900)


module Data.Units.NonSI.LengthSpec where

import Test.Hspec

import Data.Units.NonSI.Length
import Data.Units.SI

import Data.Units.Core.ConvertProp

spec :: Spec
spec = do
  describe "Length" $ do
    toFromSpec @Foot    @Double
    fromToAssert @Double (Meter 1) (Foot $ 1 / (1200 / 3937))
    fromToAssert @Double (Foot 1) (Meter $ 1200 / 3937)

    toFromSpec @Twip    @Double
    fromToAssert @Double (Meter 1) (Twip $ 1 / (1200 / 3937 / 17280))
    fromToAssert @Double (Twip 1) (Meter $ 1200 / 3937 / 17280)

    toFromSpec @Thou    @Double
    fromToAssert @Double (Meter 1) (Thou $ 1 / (1200 / 3937 / 12000))
    fromToAssert @Double (Thou 1) (Meter $ 1200 / 3937 / 12000)

    toFromSpec @Barleycorn    @Double
    fromToAssert @Double (Meter 1) (Barleycorn $ 1 / (1200 / 3937 / 36))
    fromToAssert @Double (Barleycorn 1) (Meter $ 1200 / 3937 / 36)

    toFromSpec @Inch    @Double
    fromToAssert @Double (Meter 1) (Inch $ 1 / (1200 / 3937 / 12))
    fromToAssert @Double (Inch 1) (Meter $ 1200 / 3937 / 12)

    toFromSpec @Hand    @Double
    fromToAssert @Double (Meter 1) (Hand $ 1 / (1200 / 3937 / 3))
    fromToAssert @Double (Hand 1) (Meter $ 1200 / 3937 / 3)

    toFromSpec @Yard    @Double
    fromToAssert @Double (Meter 1) (Yard $ 1 / (1200 / 3937 * 3))
    fromToAssert @Double (Yard 1) (Meter $ 1200 / 3937 * 3)

    toFromSpec @Chain    @Double
    fromToAssert @Double (Meter 1) (Chain $ 1 / (1200 / 3937 * 66))
    fromToAssert @Double (Chain 1) (Meter $ 1200 / 3937 * 66)

    toFromSpec @Furlong    @Double
    fromToAssert @Double (Meter 1) (Furlong $ 1 / (1200 / 3937 * 660))
    fromToAssert @Double (Furlong 1) (Meter $ 1200 / 3937 * 660)

    toFromSpec @Mile    @Double
    fromToAssert @Double (Meter 1) (Mile $ 1 / (1200 / 3937 * 5280))
    fromToAssert @Double (Mile 1) (Meter $ 1200 / 3937 * 5280)

    toFromSpec @League    @Double
    fromToAssert @Double (Meter 1) (League $ 1 / (1200 / 3937 * 15840))
    fromToAssert @Double (League 1) (Meter $ 1200 / 3937 * 15840)

    toFromSpec @Fathom    @Double
    fromToAssert @Double (Meter 1) (Fathom $ 1 / (1200 / 3937 * 6.0761))
    fromToAssert @Double (Fathom 1) (Meter $ 1200 / 3937 * 6.0761)

    toFromSpec @Cable    @Double
    fromToAssert @Double (Meter 1) (Cable $ 1 / (1200 / 3937 * 607.61))
    fromToAssert @Double (Cable 1) (Meter $ 1200 / 3937 * 607.61)

    toFromSpec @NauticalMile    @Double
    fromToAssert @Double (Meter 1) (NauticalMile $ 1 / (1200 / 3937 * 60761 / 10))
    fromToAssert @Double (NauticalMile 1) (Meter $ 1200 / 3937 * 60761 / 10)

    toFromSpec @Link    @Double
    fromToAssert @Double (Meter 1) (Link $ 1 / (1200 / 3937 * 66 / 100))
    fromToAssert @Double (Link 1) (Meter $ 1200 / 3937 * 66 / 100)

    toFromSpec @Rod    @Double
    fromToAssert @Double (Meter 1) (Rod $ 1 / (1200 / 3937 * 66 / 4))
    fromToAssert @Double (Rod 1) (Meter $ 1200 / 3937 * 66 / 4)

    toFromSpec @Thou    @Double
    fromToAssert @Double (Meter 1) (Thou $ 1 / (1200 / 3937 / 12000))
    fromToAssert @Double (Thou 1) (Meter $ 1200 / 3937 / 12000)

    toFromSpec @Barleycorn    @Double
    fromToAssert @Double (Meter 1) (Barleycorn $ 1 / (1200 / 3937 / 36))
    fromToAssert @Double (Barleycorn 1) (Meter $ 1200 / 3937 / 36)

    toFromSpec @Inch    @Double
    fromToAssert @Double (Meter 1) (Inch $ 1 / (1200 / 3937 / 12))
    fromToAssert @Double (Inch 1) (Meter $ 1200 / 3937 / 12)

    toFromSpec @Hand    @Double
    fromToAssert @Double (Meter 1) (Hand $ 1 / (1200 / 3937 / 3))
    fromToAssert @Double (Hand 1) (Meter $ 1200 / 3937 / 3)

    toFromSpec @Yard    @Double
    fromToAssert @Double (Meter 1) (Yard $ 1 / (1200 / 3937 * 3))
    fromToAssert @Double (Yard 1) (Meter $ 1200 / 3937 * 3)

    toFromSpec @Chain    @Double
    fromToAssert @Double (Meter 1) (Chain $ 1 / (1200 / 3937 * 66))
    fromToAssert @Double (Chain 1) (Meter $ 1200 / 3937 * 66)

    toFromSpec @Furlong    @Double
    fromToAssert @Double (Meter 1) (Furlong $ 1 / (1200 / 3937 * 660))
    fromToAssert @Double (Furlong 1) (Meter $ 1200 / 3937 * 660)

    toFromSpec @Mile    @Double
    fromToAssert @Double (Meter 1) (Mile $ 1 / (1200 / 3937 * 5280))
    fromToAssert @Double (Mile 1) (Meter $ 1200 / 3937 * 5280)

    toFromSpec @League    @Double
    fromToAssert @Double (Meter 1) (League $ 1 / (1200 / 3937 * 15840))
    fromToAssert @Double (League 1) (Meter $ 1200 / 3937 * 15840)

    toFromSpec @Fathom    @Double
    fromToAssert @Double (Meter 1) (Fathom $ 1 / (1200 / 3937 * 6.0761))
    fromToAssert @Double (Fathom 1) (Meter $ 1200 / 3937 * 6.0761)

    toFromSpec @Cable    @Double
    fromToAssert @Double (Meter 1) (Cable $ 1 / (1200 / 3937 * 607.61))
    fromToAssert @Double (Cable 1) (Meter $ 1200 / 3937 * 607.61)

    toFromSpec @NauticalMile    @Double
    fromToAssert @Double (Meter 1) (NauticalMile $ 1 / (1200 / 3937 * 60761 / 10))
    fromToAssert @Double (NauticalMile 1) (Meter $ 1200 / 3937 * 60761 / 10)

    toFromSpec @Link    @Double
    fromToAssert @Double (Meter 1) (Link $ 1 / (1200 / 3937 * 66 / 100))
    fromToAssert @Double (Link 1) (Meter $ 1200 / 3937 * 66 / 100)

    toFromSpec @Rod    @Double
    fromToAssert @Double (Meter 1) (Rod $ 1 / (1200 / 3937 * 66 / 4))
    fromToAssert @Double (Rod 1) (Meter $ 1200 / 3937 * 66 / 4)

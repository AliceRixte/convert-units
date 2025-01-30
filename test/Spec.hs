import Test.Hspec
import Test.QuickCheck
import Data.Fixed
import Pandia.Space.Geometry.Angle
import Pandia.Space.Util.Epsilon

import qualified Linear as Lin

instance Arbitrary a => Arbitrary (Lin.Quaternion a) where
  arbitrary = Lin.Quaternion <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Lin.V3 a) where
  arbitrary = Lin.V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Radians a) where
  arbitrary = Radians <$> arbitrary

instance Arbitrary a => Arbitrary (Cardan a) where
  arbitrary = Cardan <$> arbitrary <*> arbitrary <*> arbitrary


aboutEqCardan :: Lin.Epsilon a =>
  Cardan (Radians a) -> Cardan (Radians a) -> Bool
aboutEqCardan (Cardan x y z) (Cardan x' y' z') =
  aboutEqual x x' && aboutEqual y y' && aboutEqual z z'

eulerToEuler :: Cardan (Radians Double) -> Bool
eulerToEuler card =
  let card'@(Cardan _ ayaw' _) = normalizeRadians <$> card  in

  -- when beyond, gimbal lock issues, in which case we validate the test
  ayaw' > pi/2 || ayaw' < -pi/2 ||
    aboutEqCardan (gimbalAngles $ quaternionToGimbal $ cardanToQuaternion card')
                card'


main :: IO ()
main = hspec $ do
  describe "normalizeRadians" $ do
    it "0" $ normalizeRadians 0 `shouldBe` 0
    it "pi" $ normalizeRadians pi `shouldBe` pi
    it "2pi" $ normalizeRadians (2 * pi) `shouldBe` 0
    it "5pi/4" $ normalizeRadians (5 * pi / 4) `shouldBe` (-3 * pi/4)
    it "-2pi" $ normalizeRadians (-2 * pi) `shouldBe` 0
    it "-5pi/4" $ normalizeRadians (-5 * pi / 4) `shouldBe` (3 * pi /4)
    it "-9pi/4" $ normalizeRadians (-9 * pi / 4) `shouldBe` (- pi /4)

  describe "Angle" $ do
    describe "Conversion euler corresponds to axisAngle" $  do
      it "x" $ property $ \ x ->
        Lin.axisAngle (Lin.V3 1 0 0) x `shouldBe`
          cardanToQuaternion (Cardan (Radians (x :: Double)) 0 0)
      it "y" $ property $ \ y ->
        Lin.axisAngle (Lin.V3 0 1 0) y `shouldBe`
          cardanToQuaternion (Cardan  0 (Radians (y :: Double)) 0)
      it "z" $ property $ \ z ->
        Lin.axisAngle (Lin.V3 0 0 1) z `shouldBe`
          cardanToQuaternion (Cardan 0 0 (Radians (z :: Double)))
    it "Conversion euler/quaternions" $ property eulerToEuler
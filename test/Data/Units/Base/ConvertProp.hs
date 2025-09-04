{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.Base.ConvertProp where


import Data.Coerce

import Data.Epsilon

import Test.QuickCheck
import Test.Hspec

import Data.Units

------------------------------------ toFrom ------------------------------------

toFrom :: forall u a.
  (ConvertibleUnit u a)
  => a -> a
toFrom a = coerce
  (toBaseUnit (fromBaseUnit @u (coerce a :: BaseUnitOf u a) :: u a) :: BaseUnitOf u a)

toFromProp :: forall u a.
  ( ConvertibleUnit u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
toFromProp = property (isApproxId (toFrom @u @a))

toFromSpec :: forall u a.
  ( ShowUnit u
  , ConvertibleUnit u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Spec
toFromSpec =
  it ("toFrom " ++ showUnit @u) (toFromProp @u @a)


------------------------------------ fromTo ------------------------------------

fromToRoundtrip :: forall u v a.
  ( IsUnit u, IsUnit v
  , FromTo u v a, FromTo v u a
  )
  => a -> a
fromToRoundtrip a = coerce (fromTo (fromTo (coerce a :: u a) :: v a) :: u a)

fromToProp :: forall u v a.
  ( ShowUnit u, ShowUnit v
  , FromTo u v a, FromTo v u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
fromToProp = property $ isApproxId (fromToRoundtrip @u @v @a)

fromToSpec :: forall u v a.
  ( ShowUnit u, ShowUnit v
  , FromTo u v a, FromTo v u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Spec
fromToSpec =
  it ("From `" ++ showUnit @u ++ "` to `" ++ showUnit @v ++ "`")
    $ fromToProp @u @v @a

fromToAssert :: forall a u v.
  ( Show (u a), Show (v a)
  , FromTo u v a
  , FromTo v u a
  , Arbitrary a, ShowUnit u, ShowUnit v, Show a, Epsilon a
  )
  => u a -> v a -> Spec
fromToAssert u v =
  it ("`" ++ prettyQuantity u ++ "` should be `" ++ prettyQuantity v ++ "`") $
    aboutEqual (coerce (fromTo u :: v a) :: a) (coerce v :: a)
      && aboutEqual (coerce (fromTo v :: u a) :: a) (coerce u :: a)
      `shouldBe` True


----------------------------------- fromTo' ------------------------------------

fromToRoundtrip' :: forall u v a.
  ( FromTo' u v a, FromTo' v u a)
  => a -> a
fromToRoundtrip' a = coerce (fromTo' (fromTo' (coerce a :: u a) :: v a) :: u a)

fromToProp' :: forall u v a.
  ( ShowUnit u, ShowUnit v
  , FromTo' u v a, FromTo' v u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
fromToProp' = property $ isApproxId (fromToRoundtrip' @u @v @a)


fromToSpec' :: forall u v a.
  ( ShowUnit u, ShowUnit v
  , FromTo' u v a, FromTo' v u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Spec
fromToSpec' =
  it ("From' `" ++ showUnit @u ++ "` to' `" ++ showUnit @v ++ "`")
    $ fromToProp' @u @v @a
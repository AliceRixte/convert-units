{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.Common where
import Data.Coerce


import Data.Epsilon

import Test.QuickCheck
import Test.Hspec

import Data.Units

------------------------------------ toFrom ------------------------------------

toFrom :: forall u a.
  (From u a, To u a)
  => a -> a
toFrom a = coerce
  (from (to @u (coerce a :: StdUnitOf u a) :: u a) :: StdUnitOf u a)

toFromProp :: forall u a.
  ( From u a, To u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
toFromProp = property (isApproxId (toFrom @u @a))

toFromSpec :: forall u a.
  ( ShowUnit u
  , From u a, To u a
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

------------------ Arithmetic with two different dimensions  -------------------

mulDiffDim :: forall u v a.
  ( ConvFactor u a, ConvFactor v a
  , IsUnit (NormalizeUnit (u -*- v))
  )
  => a -> a -> a
mulDiffDim u v = coerce $ (coerce u :: u a) -*- (coerce v :: v a)

mulDiffDimProp :: forall u v a.
  ( ConvFactor u a, ConvFactor v a
  , IsUnit (NormalizeUnit (u -*- v))
  , Coercible a (StdUnitOf (NormalizeUnit (u -*- v)) a)
  , ConvFactor (NormalizeUnit (u -*- v)) a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
mulDiffDimProp =
  property (\a b -> aboutEqual (a * b) (mulDiffDim @u @v @a a b) )
  .&&. toFromProp @(NormalizeUnit (u -*- v)) @a

mulDiffDimSpec :: forall u v a.
  ( ConvFactor u a, ConvFactor v a
  , IsUnit (NormalizeUnit (u -*- v))
  , Coercible a (StdUnitOf (NormalizeUnit (u -*- v)) a)
  , ConvFactor (NormalizeUnit (u -*- v)) a
  , Arbitrary a, Show a, Epsilon a
  , ShowUnit u, ShowUnit v
  )
  => Spec
mulDiffDimSpec = it (showUnit @u ++ " -*- " ++ showUnit @v)
    $ mulDiffDimProp @u @v @a

divDiffDim :: forall u v a.
  ( ConvFactor u a, ConvFactor v a
  )
  => a -> a -> a
divDiffDim u v = coerce $ (coerce u :: u a) -/- (coerce v :: v a)

divDiffDimProp :: forall u v a.
  ( ConvFactor u a, ConvFactor (InverseUnit v) a, ConvFactor v a
  , IsUnit (StdUnitOf (u -/- v))
  , Arbitrary a, Show a, Epsilon a, Eq a
  )
  => Property
divDiffDimProp =
  property (\a b -> b == 0 || aboutEqual (a / b) (divDiffDim @u @v @a a b) )
  .&&. toFromProp @(u -/- v) @a

divDiffDimSpec :: forall u v a.
  ( ConvFactor u a, ConvFactor (InverseUnit v) a, ConvFactor v a
  , IsUnit (StdUnitOf (u -/- v))
  , Arbitrary a, Show a, Epsilon a, Eq a
  , ShowUnit u, ShowUnit v
  )
  => Spec
divDiffDimSpec =  it (showUnit @u ++ " -/- " ++ showUnit @v)
  $ divDiffDimProp @u @v @a


-------------------------- Arithmetic same dimension ---------------------------

addLeft :: forall u v a.
  ( ConvFactor u a, ConvFactor v a
  , From u a
  , DimEq u v
  )
  => a -> a -> a
addLeft u v = coerce $ (coerce u :: u a) ~+- (coerce v :: v a)

-- addLeftProp :: forall u v a.
--   ( From u a, To v a
--   , DimEq u v
--   , Arbi
--   )
--   => Property
-- addLeftProp = property (\a b ->
--   aboutEqual (coerce (fromTo (coerce a :: u a) :: v a) + b)
--              (mulDiffDim @u @v @a a b) )
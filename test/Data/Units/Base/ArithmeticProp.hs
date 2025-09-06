{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Data.Units.Base.ArithmeticProp where

import Data.Coerce

import Test.QuickCheck
import Test.Hspec

import Data.Epsilon

import Data.Units

import Data.Units.Base.ConvertProp


------------------------- Num and fractional instances -------------------------

addNumProp :: forall u a.
  ( IsUnit u, Floating (u a)
  , Show a, Epsilon a, Floating a, Eq a, Arbitrary a) =>  Property
addNumProp = property (\ (a :: a) (b :: a) ->
  a + b + pi ==
    coerce (coerce a + coerce b + pi :: u a)
  )

divNumProp :: forall u a.
 ( IsUnit u, Fractional (u a)
 , Show a, Epsilon a, Fractional a, Eq a, Arbitrary a)
 => Property
divNumProp = property (\ (a :: a) (b :: a) -> b == 0 ||
  a / b ==
    coerce (coerce a / coerce b :: u a))

floatingSpec :: forall u a.
  ( IsUnit u, Floating (u a), ShowUnit u
  , Show a, Epsilon a, Floating a, Eq a, Arbitrary a
  )
  =>  Spec
floatingSpec = it ("Floating instance for " ++ showUnit @u ) $
  addNumProp @u @a .&&. divNumProp @u @a


-------------------------- Arithmetic same dimension ---------------------------



addRight :: forall u v a.
  ( FromTo' v u a
  )
  => a -> a -> a
addRight u v = coerce $ (coerce u :: u a) .+~ (coerce v :: v a)

addRightProp :: forall u v a.
  ( FromTo' v u a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
addRightProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) + b :: a)
             (addRight @u @v a b))

addRightSpec :: forall u v a.
  ( FromTo' v u a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
addRightSpec = it (showUnit @u ++ " .+~ " ++ showUnit @v)
  $ addRightProp @u @v @a

addLeft :: forall u v a.
  ( FromTo' u v a
  )
  => a -> a -> a
addLeft u v = coerce $ (coerce u :: u a) ~+. (coerce v :: v a)

addLeftProp :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
addLeftProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) + b :: a)
             (addLeft @u @v a b))

addLeftSpec :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
addLeftSpec = it (showUnit @u ++ " ~+. " ++ showUnit @v)
  $ addLeftProp @u @v @a


addSame :: forall u v a.
  FromTo' u v a
  => a -> a -> a
addSame u v = coerce $ (coerce u :: u a) ~+~ (coerce v :: v a)

addSameProp :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
addSameProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) + b :: a)
             (addSame @u @v a b))

addSameSpec :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
addSameSpec = it (showUnit @u ++ " ~+~ " ++ showUnit @v)
  $ addSameProp @u @v @a

--------------------------------- Subtraction ----------------------------------

subRight :: forall u v a.
  ( FromTo' v u a
  )
  => a -> a -> a
subRight u v = coerce $ (coerce u :: u a) .-~ (coerce v :: v a)

subRightProp :: forall u v a.
  ( FromTo' v u a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
subRightProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) - b :: a)
             (subRight @u @v a b))

subRightSpec :: forall u v a.
  ( FromTo' v u a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
subRightSpec = it (showUnit @u ++ " .-~ " ++ showUnit @v)
  $ subRightProp @u @v @a

subLeft :: forall u v a.
  ( FromTo' u v a
  )
  => a -> a -> a
subLeft u v = coerce $ (coerce u :: u a) ~-. (coerce v :: v a)

subLeftProp :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
subLeftProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) - b :: a)
             (subLeft @u @v a b))

subLeftSpec :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
subLeftSpec = it (showUnit @u ++ " ~-. " ++ showUnit @v)
  $ subLeftProp @u @v @a


subSame :: forall u v a.
  FromTo' u v a
  => a -> a -> a
subSame u v = coerce $ (coerce u :: u a) ~-~ (coerce v :: v a)

subSameProp :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
subSameProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) - b :: a)
             (subSame @u @v a b))

subSameSpec :: forall u v a.
  ( FromTo' u v a
  , DimEq u v
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  ) => Spec
subSameSpec = it (showUnit @u ++ " ~-~ " ++ showUnit @v)
  $ subSameProp @u @v @a


-------------------------------- Multiplication --------------------------------

mulSame :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  )
  => a -> a -> a
mulSame u v = coerce $ (coerce u :: u a) ~*~ (coerce v :: v a)

mulSameProp :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  , Show a, Epsilon a, Arbitrary a
  )
  => Property
mulSameProp = property (\(a :: a) (b :: a) ->
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) * b :: a)
             (mulSame @u @v a b))

mulSameSpec :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Arbitrary a
  )
  => Spec
mulSameSpec = it (showUnit @u ++ " ~*~ " ++ showUnit @v)
  $ mulSameProp @u @v @a

divSame :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  )
  => a -> a -> a
divSame u v = coerce $ (coerce u :: u a) ~/~ (coerce v :: v a)

divSameProp :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  , Show a, Epsilon a, Eq a, Arbitrary a
  )
  => Property
divSameProp = property (\(a :: a) (b :: a) -> b == 0 ||
  aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) / b :: a)
             (divSame @u @v a b))

divSameSpec :: forall u v a u2 .
  ( u2 ~ NormalizeUnit u .^+ 2, IsUnit u2
  , DimEq u v
  , ConversionFactor u a, ConversionFactor v a
  , ShowUnit u, ShowUnit v
  , Show a, Epsilon a, Eq a, Arbitrary a
  )
  => Spec
divSameSpec = it (showUnit @u ++ " ~/~ " ++ showUnit @v)
  $ divSameProp @u @v @a

------------------ Arithmetic with two different dimensions  -------------------

-- mulRight :: forall u v a u2.
--   ( u2 ~ NormalizeUnit' (u .^+ 2) , IsUnit u2
--   , DimEq u v
--   , FromTo' v u a
--   )
--   => a -> a -> a
-- mulRight u v = coerce $ (coerce u :: u a) .*~ (coerce v :: v a)

-- mulRightProp :: forall u v a u2.
--   ( u2 ~ NormalizeUnit' (u .^+ 2) , IsUnit u2
--   , DimEq u v
--   , FromTo' v u a
--   , Show a, Epsilon a, Arbitrary a
--   )
--   => Property
-- mulRightProp = property (\(a :: a) (b :: a) ->
--   aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) * b :: a)
--              (mulRight @u @v a b))

-- mulRightSpec :: forall u v a u2.
--   ( u2 ~ NormalizeUnit' (u .^+ 2) , IsUnit u2
--   , DimEq u v
--   , FromTo' v u a
--   , ShowUnit u, ShowUnit v
--   , Show a, Epsilon a, Arbitrary a
--   ) => Spec
-- mulRightSpec = it (showUnit @u ++ " .*~ " ++ showUnit @v)
--   $ mulRightProp @u @v @a

-- mulLeft :: forall u v a v2.
--   ( v2 ~ NormalizeUnit' (v .^+ 2), IsUnit v2
--   , DimEq v u
--   , FromTo' u v a
--   )
--   => a -> a -> a
-- mulLeft u v = coerce $ (coerce u :: u a) ~*. (coerce v :: v a)

-- mulLeftProp :: forall u v a v2.
--   ( v2 ~ NormalizeUnit' (v .^+ 2), IsUnit v2
--   , DimEq v u
--   , FromTo' u v a
--   , Show a, Epsilon a, Arbitrary a
--   )
--   => Property
-- mulLeftProp = property (\(a :: a) (b :: a) ->
--   aboutEqual (coerce (fromTo' (coerce a :: u a) :: v a) * b :: a)
--              (mulLeft @u @v a b))

-- mulLeftSpec :: forall u v a v2.
--   ( v2 ~ NormalizeUnit' (v .^+ 2), IsUnit v2
--   , DimEq v u
--   , FromTo' u v a
--   , ShowUnit u, ShowUnit v
--   , Show a, Epsilon a, Arbitrary a
--   ) => Spec
-- mulLeftSpec = it (showUnit @u ++ " ~*. " ++ showUnit @v)
--   $ mulLeftProp @u @v @a


mulDiffDim :: forall u v a.
  ( ConversionFactor u a, ConversionFactor v a)
  => a -> a -> a
mulDiffDim u v = coerce $ (coerce u :: u a) .*. (coerce v :: v a)

mulDiffDimProp :: forall u v a.
  ( ConversionFactor u a, ConversionFactor v a
  , ConversionFactor (u .*. v) a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
mulDiffDimProp =
  property (\a b -> aboutEqual (a * b) (mulDiffDim @u @v @a a b) )
  .&&. toFromProp @(u .*. v) @a

mulDiffDimSpec :: forall u v a.
  ( ConversionFactor u a, ConversionFactor v a
  , ConversionFactor (u .*. v) a
  , Arbitrary a, Show a, Epsilon a
  , ShowUnit u, ShowUnit v
  )
  => Spec
mulDiffDimSpec = it (showUnit @u ++ " .*. " ++ showUnit @v)
    $ mulDiffDimProp @u @v @a

divDiffDim :: forall u v a uv .
  ( IsUnit u, IsUnit v, IsUnit (u ./. v)
  , Fractional a
  )
  => a -> a -> a
divDiffDim u v = coerce $ (coerce u :: u a) ./. (coerce v :: v a)

divDiffDimProp :: forall u v a uv.
  ( IsUnit u, IsUnit v, IsUnit (u ./. v)
  , ConversionFactor (u ./. v) a
  , IsUnit (u ./. v)
  , Arbitrary a, Show a, Epsilon a, Eq a, Fractional a
  )
  => Property
divDiffDimProp =
  property (\a b -> b == 0 || aboutEqual (a / b) (divDiffDim @u @v @a a b) )
  .&&. toFromProp @(u ./. v) @a

divDiffDimSpec :: forall u v a uv.
  (IsUnit u, IsUnit v, IsUnit (u ./. v)
  , ConversionFactor (u ./. v) a
  , IsUnit (u ./. v)
  , Arbitrary a, Show a, Epsilon a, Eq a, Fractional a
  , ShowUnit u, ShowUnit v
  )
  => Spec
divDiffDimSpec =  it (showUnit @u ++ " ./. " ++ showUnit @v)
  $ divDiffDimProp @u @v @a

-------------------------------- Exponentiation --------------------------------

exp2 :: forall u a.
  ConversionFactor u a
  => a -> a
exp2 u = coerce $ (coerce u :: u a) .^. pos2

expm1 :: forall u a.
  ConversionFactor u a
  => a -> a
expm1 u = coerce $ (coerce u :: u a) .^. neg1

exp2Prop :: forall u a.
  ( ConversionFactor u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
exp2Prop =
  property (\a -> aboutEqual (a * a) (exp2 @u @a a))

expm1Prop :: forall u a.
  ( ConversionFactor u a
  , Arbitrary a, Show a,  Eq a, Epsilon a
  )
  => Property
expm1Prop =
  property (\a -> a == 0 || aboutEqual (1 / a) (expm1 @u @a a))

expSpec :: forall u a.
  ( ConversionFactor u a
  , ShowUnit u
  , Arbitrary a, Show a,  Eq a, Epsilon a
  )
  => Spec
expSpec = it (showUnit @u ++ " .^+ 2  ," ++ showUnit @u ++ " .^- 1") $
  exp2Prop @u @a .&&. expm1Prop @u @a

exp2Conv :: forall u a.
  ConversionFactor u a
  => a -> a
exp2Conv u = coerce $ (coerce u :: u a) ~^~ pos2

expm1Conv :: forall u a.
  ConversionFactor u a
  => a -> a
expm1Conv u = coerce $ (coerce u :: u a) ~^~ neg1

exp2ConvProp :: forall u a.
  ( ConversionFactor u a
  , Arbitrary a, Show a, Epsilon a
  )
  => Property
exp2ConvProp =
  property (\a -> aboutEqual
    (coerce (toNormalUnit' (coerce a :: u a)) ^^ (2 :: Int)) (exp2Conv @u @a a))

expm1ConvProp :: forall u a.
  ( ConversionFactor u a
  , Arbitrary a, Show a, Eq a, Epsilon a
  )
  => Property
expm1ConvProp =
  property (\a -> a == 0 ||
    aboutEqual (coerce (toNormalUnit' (coerce a :: u a)) ^^ (-1 :: Int)) (expm1Conv @u @a a))


expConvSpec :: forall u a.
  ( ConversionFactor u a
  , ShowUnit u
  , Arbitrary a, Show a,  Eq a, Epsilon a
  )
  => Spec
expConvSpec = it (showUnit @u ++ "~^~ pos2  ," ++ showUnit @u ++ "~^~ neg1") $
  exp2ConvProp @u @a .&&. expm1ConvProp @u @a


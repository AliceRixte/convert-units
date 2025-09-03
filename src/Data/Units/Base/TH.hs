
{-# LANGUAGE TemplateHaskell #-}


--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.Base.TH
-- Description :  Template Haskell quasi quoter for unit declaration
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
--------------------------------------------------------------------------------


module Data.Units.Base.TH where

import GHC.TypeError

import Language.Haskell.TH

import Data.Units.Base.System
import Data.Units.Base.Convert

-- | List of derived class for a unit.
--
deriveList :: [Name]
deriveList =
  [ ''Show
  , ''Eq
  , ''Ord
  , ''Num
  , ''Fractional
  , ''Floating
  , ''Real
  , ''RealFrac
  , ''RealFloat
  ]

-- | Make a newtype of the form
--
-- @
-- newtype Minute a = Minute a
--   deriving ( Show, Eq, Ord, Num, Fractional, Floating, Real
--            , RealFrac, RealFloat)
-- @
--
mkUnitNewtype :: Quote m
  => [Name] -> Name -> m Dec
mkUnitNewtype l unitName =
  let a = varT (mkName "a") in
  newtypeD
    (cxt [])
    unitName
    [PlainTV (mkName "a") BndrReq]
    Nothing
    (normalC unitName
      [bangType (bang noSourceUnpackedness noSourceStrictness) a])
    [ derivClause Nothing (conT <$> l)]

-- | Make instance of the form
--
-- @
-- instance IsUnit Hour where
--   type DimOf Hour = Time
-- @
--
mkIsUnitInstance :: Quote m => Name -> Name -> m Dec
mkIsUnitInstance unitName dimName =
  instanceD
    (cxt [])
    [t| IsUnit $(conT unitName) |]
    [tySynInstD (tySynEqn Nothing
      [t| DimOf $(conT unitName) |]
      (conT dimName))]

-- | Make an instance of the form
--
-- @
-- instance ShowUnit Minute where
--   type ShowUnitType Minute = Text "min"
--   showUnit = "Minute"
--   prettyUnit = "min"
-- @
--
mkShowUnitInstance :: Quote m => Name -> String -> String -> m Dec
mkShowUnitInstance unitName unitStr prettyStr  =
  instanceD
    (cxt [])
    [t| ShowUnit $(conT unitName) |]
    [ tySynInstD (tySynEqn Nothing
        [t| ShowUnitType $(conT unitName) |]
        (promotedT 'Text `appT` return (LitT (StrTyLit prettyStr))))
    , funD 'showUnit [clause [] (normalB (litE (StringL unitStr))) []]
    , funD 'prettyUnit [clause [] (normalB (litE (StringL prettyStr))) []]
    ]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvFactor Minute a where
--    factorFrom = 60
-- @
--
mkConvFactorInstance :: Quote m => Name -> Rational -> m Dec
mkConvFactorInstance unitName factor =
  let a = varT (mkName "a") in
  instanceD
    (cxt [appT (conT ''Fractional) a])
    (conT ''ConvFactor `appT` conT unitName `appT` a)
    [ funD 'factorFrom [clause [] (normalB (litE (RationalL factor))) []] ]

-- | Make a unit that can be converted via a factor
--
-- [Usage:]
--
-- @
-- \$(mkUnit "Minute" "min" ''Time 60)
-- \$(mkUnit "Celsius" "°C" ''Temperature 1)
-- @
--
-- [Note:]
--
-- It is possibe to overload
--
mkUnit :: String -> String -> Name -> Rational -> Q [Dec]
mkUnit unitStr prettyStr dimName  factor = do
  let unitName = mkName unitStr
  newtypeDec  <- mkUnitNewtype deriveList unitName
  isUnitDec  <- mkIsUnitInstance unitName dimName
  showUnitDec  <- mkShowUnitInstance unitName unitStr prettyStr
  convFactorDec  <- mkConvFactorInstance unitName factor
  return [newtypeDec, isUnitDec, showUnitDec, convFactorDec]

-- | Make a unit without declaring any conversion instances.
--
-- Conversion instances must be added by hand.
--
-- [Usage:]
--
-- @
-- \$(mkUnit \"Bel\" "B" ''NoUnit )
-- @
--
mkUnitNoFactor :: String -> String -> Name -> Q [Dec]
mkUnitNoFactor unitStr prettyStr dimName = do
  let unitName = mkName unitStr
  newtypeDec  <- mkUnitNewtype deriveList unitName
  isUnitDec  <- mkIsUnitInstance unitName dimName
  showUnitDec  <- mkShowUnitInstance unitName unitStr prettyStr
  return [newtypeDec, isUnitDec, showUnitDec]



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


module Data.Units.Base.TH
  ( -- * Units
    mkUnit
  , mkUnitNoFactor
  , mkBaseUnit
  -- * Dimensions
  , mkDim
  -- * Prefixes
  , mkPrefix
  )
  where

import GHC.TypeError

import Language.Haskell.TH

import Data.Type.Int
import Data.Units.Base.System
import Data.Units.Base.Convert
import Data.Units.Base.Prefix

------------------------------------ Units -------------------------------------

-- | List of derived class for a unit.
--
deriveList :: [Name]
deriveList =
  [''Eq
  , ''Ord
  , ''Num
  , ''Fractional
  , ''Floating
  , ''Real
  , ''RealFrac
  , ''RealFloat
  ]

-- | List of derived classes for a unit.
--
deriveListUnit :: [Name]
deriveListUnit = ''Show : deriveList

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
mkIsUnitInstance :: Quote m => Name -> Name -> m [Dec]
mkIsUnitInstance unitName dimName = [d|
  instance IsUnit $(conT unitName) where
    type DimOf $(conT unitName) = $(conT dimName)
  |]

-- | Make an instance of the form
--
-- @
-- instance ShowUnit Minute where
--   type ShowUnitType Minute = Text "min"
--   showUnit = "Minute"
--   prettyUnit = "min"
-- @
--
mkShowUnitInstance :: Quote m => Name -> String -> String -> m [Dec]
mkShowUnitInstance unitName unitStr prettyStr = [d|
  instance ShowUnit $(conT unitName) where
    type ShowUnitType $(conT unitName) = Text $(pure (LitT (StrTyLit prettyStr)))
    showUnit  = $(litE (StringL unitStr))
    prettyUnit = $(litE (StringL prettyStr))
  |]


-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConversionFactor Minute a where
--    factor = 60
-- @
--
mkConvFactorInstance :: Quote m => Name -> Rational -> m [Dec]
mkConvFactorInstance unitName fctr = [d|
  instance Fractional a => ConversionFactor $(conT unitName) a where
    factor = $(litE (RationalL fctr))
  |]


-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvertibleUnit Minute a
-- @
--
mkDefaultSigConvertibleInstance :: Quote m => Name -> m [Dec]
mkDefaultSigConvertibleInstance unitName = [d|
  instance Fractional a => ConvertibleUnit $(conT unitName) a
  |]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvertibleUnit Meter a where
--    toBaseUnit = coerce
--    {-# INLINE toBaseUnit #-}
--    fromBaseUnit = coerce
--    {-# INLINE fromBaseUnit #-}
-- @
--
mkNormalConvertibleInstance :: Quote m => Name -> m [Dec]
mkNormalConvertibleInstance unitName = [d|
  instance Fractional a => ConvertibleUnit $(conT unitName) a where
    toBaseUnit = coerce
    {-# INLINE toBaseUnit #-}
    fromBaseUnit = coerce
    {-# INLINE fromBaseUnit #-}
  |]


-- | Make a unit that can be converted via a factor
--
-- [Usage:]
--
-- @
-- \$(mkUnit "Minute" "min" ''Time 60)
-- @
--
mkUnit :: String -> String -> Name -> Rational -> Q [Dec]
mkUnit unitStr prettyStr dimName fctr = do
  let unitName = mkName unitStr
  newtypeDec <- mkUnitNewtype deriveListUnit unitName
  isUnitDec <- mkIsUnitInstance unitName dimName
  showUnitDec <- mkShowUnitInstance unitName unitStr prettyStr
  convFactorDec <- mkConvFactorInstance unitName fctr
  convUnitDec <-
    if fctr == 1 then
      mkNormalConvertibleInstance unitName
    else
      mkDefaultSigConvertibleInstance unitName
  return $
    [newtypeDec] ++ isUnitDec ++ showUnitDec ++ convFactorDec ++ convUnitDec

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
  newtypeDec  <- mkUnitNewtype deriveListUnit unitName
  isUnitDec  <- mkIsUnitInstance unitName dimName
  showUnitDec  <- mkShowUnitInstance unitName unitStr prettyStr
  return $ [newtypeDec] ++  isUnitDec ++ showUnitDec


-- | Make a base unit.
--
-- In addition to calling 'mkUnit' with factor 1, this also makes an
-- instance of 'IsDim', which cannot be done in 'mkDim' since the base unit is
-- not yet declared.
--
-- [Usage:]
--
-- @ \$(mkBaseUnit "Second" "s" ''Time ) @
--
mkBaseUnit :: String -> String -> Name -> Q [Dec]
mkBaseUnit unitStr prettyStr dimName = do
  let unitName = mkName unitStr
  unitDec <- mkUnit unitStr prettyStr dimName 1
  isDimDec <- mkIsDimInstance dimName unitName
  return $ unitDec ++ isDimDec

---------------------------------- Dimensions ----------------------------------

-- | List of derived classes for a dimension.
--
deriveListDim :: [Name]
deriveListDim = deriveListUnit


mkDimNewtype :: Quote m
  => [Name] -> Name -> m Dec
mkDimNewtype = mkUnitNewtype

-- | Make an instance of the form
--
-- @
-- instance IsDim Time where
--   type DimToUnit Time = Second
-- @
--
mkIsDimInstance :: Quote m => Name -> Name -> m [Dec]
mkIsDimInstance dimName unitName = [d|
  instance IsDim $(conT dimName) where
    type DimToUnit $(conT dimName) = $(conT unitName)
  |]

-- | Make a type instance of the form
--
-- @
-- type instance DimId Time = 400
-- @
--
mkDimIdTypeInstance :: Quote m => Name -> Integer -> m [Dec]
mkDimIdTypeInstance dimName n = [d|
  type instance DimId $(conT dimName) = Pos $(litT (numTyLit n))
  |]

-- | Make a type instance of the form
--
-- @
-- instance ShowDim Minute where
--   type ShowDimType Minute = Text "min"
--   showDim = "Minute"
--   prettyDim = "min"
-- @
--
mkShowDimInstance :: Quote m => Name -> String -> String -> m [Dec]
mkShowDimInstance dimName dimStr prettyStr = [d|
  instance ShowDim $(conT dimName) where
    type ShowDimType $(conT dimName) = Text $(pure (LitT (StrTyLit prettyStr)))
    showDim  = $(litE (StringL dimStr))
    prettyDim = $(litE (StringL prettyStr))
  |]

-- | Make a dimension.
--
-- This will not declare an instance for 'IsDim', which is instead declared
-- using 'mkBaseUnit'.

-- [Usage:]
--
-- @ \$(mkDim "Time" "T" 400) @
--
mkDim :: String -> String -> Integer -> Q [Dec]
mkDim dimStr prettyStr n = do
  let dimName = mkName dimStr
  newtypeDec <- mkDimNewtype deriveListDim dimName
  -- isDimDec <- mkIsDimInstance dimName unitName
  dimIdDec <- mkDimIdTypeInstance dimName n
  showDimDec <- mkShowDimInstance dimName dimStr prettyStr
  return $ [newtypeDec]  ++ dimIdDec ++ showDimDec



----------------------------------- Prefixes -----------------------------------



-- | Make a newtype of the form:
--
-- @
-- newtype Milli (u :: Unit) a = Milli (u a)
--   deriving (Eq, Ord, Num, Fractional, Floating, Real
--            , RealFrac, RealFloat, Functor)
--   deriving Show via MetaPrefix Milli u a
--   deriving ShowUnit via MetaPrefix Milli u
-- @
--
mkPrefixNewtype :: Monad m => [Name] -> Name -> m Dec
mkPrefixNewtype l prefixName = pure $
  NewtypeD
    []
    prefixName
    [ KindedTV (mkName "u") BndrReq (ConT ''Unit)
    , PlainTV  (mkName "a") BndrReq
    ]
    Nothing
    (NormalC prefixName
      [ ( Bang NoSourceUnpackedness NoSourceStrictness
        , AppT (VarT (mkName "u")) (VarT (mkName "a"))
        )
      ])
    [ DerivClause
        (Just NewtypeStrategy)
        (map ConT l)
    , DerivClause
        (Just (ViaStrategy viaType))
        [ConT ''Show]
    , DerivClause
        (Just (ViaStrategy viaTypeNoA))
        [ConT ''ShowUnit]
    ]
  where
    viaType =
      foldl AppT (ConT ''MetaPrefix)
        [ ConT prefixName
        , VarT (mkName "u")
        , VarT (mkName "a")
        ]

    viaTypeNoA =
      foldl AppT (ConT ''MetaPrefix)
        [ ConT prefixName
        , VarT (mkName "u")
        ]

-- | Make a deriving instance of the form
--
-- @
-- deriving via MetaPrefix Milli u instance IsUnit u => IsUnit (Milli u )
-- @
--
mkPrefixIsUnitInstance :: Name -> Q [Dec]
mkPrefixIsUnitInstance prefixName = [d|
  deriving via MetaPrefix $(conT prefixName) u instance IsUnit u => IsUnit ($(conT prefixName)  u)
  |]

-- | Make an instance of the form
--
-- @
-- instance ShowPrefix Milli where
--    type ShowPrefixType Milli = Text "m"
--    showPrefix = "Milli"
--    prettyPrefix = "m"
-- @
--
mkShowPrefixInstance :: Quote m => Name -> String -> String -> m [Dec]
mkShowPrefixInstance prefixName prefixStr prettyStr = [d|
  instance ShowPrefix $(conT prefixName) where
    type ShowPrefixType $(conT prefixName) = Text $(pure (LitT (StrTyLit prettyStr)))
    showPrefix = $(litE (StringL prefixStr))
    prettyPrefix = $(litE (StringL prettyStr))
  |]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => PrefixFactor Kilo a where
--   prefixFactor = 1000
--   {-# INLINE prefixFactor #-}
--
mkPrefixFactorInstance :: Name -> Rational -> Q [Dec]
mkPrefixFactorInstance prefixName fctr = [d|
  instance Fractional a => PrefixFactor $(conT prefixName) a where
    prefixFactor = $(litE (RationalL fctr))
    {-# INLINE prefixFactor #-}
  |]

-- | Make an instance of the form
--
-- @
-- instance ConversionFactor u a => ConversionFactor (Milli u) a where
--   factor = factor @(MetaPrefix Milli u)
--   {-# INLINE factor #-}
-- @
--
mkPrefixConvFactorInstance :: Name -> Q [Dec]
mkPrefixConvFactorInstance prefixName = [d|
  instance ConversionFactor u a => ConversionFactor ($(conT prefixName) u) a where
    factor = factor @(MetaPrefix $(conT prefixName) u)
    {-# INLINE factor #-}
  |]

-- | Make an instance of the form
--
-- @
-- instance (ConvertibleUnit u a, Fractional a)
--   => ConvertibleUnit (Milli u) a where
--   toBaseUnit = prefixToBaseUnit
--   {-# INLINE toBaseUnit #-}
--   fromBaseUnit = prefixFromBaseUnit
--   {-# INLINE fromBaseUnit #-}
-- @
--
mkPrefixConvUnitInstance :: Name -> Q [Dec]
mkPrefixConvUnitInstance prefixName = [d|
  instance (ConvertibleUnit u a, Fractional a)
    => ConvertibleUnit ($(conT prefixName) u) a where
    toBaseUnit = prefixToBaseUnit
    {-# INLINE toBaseUnit #-}
    fromBaseUnit = prefixFromBaseUnit
    {-# INLINE fromBaseUnit #-}
  |]

-- | Make a unit prefix.
--
-- [Usage:]
--
-- @
-- \$(mkPrefix "Kilo" "k" 1000)
-- @
--
mkPrefix :: String -> String -> Rational -> Q [Dec]
mkPrefix prefixStr prettyStr fctr = do
  let prefixName = mkName prefixStr
  newtypeDec <- mkPrefixNewtype deriveList prefixName
  isUnitDec <- mkPrefixIsUnitInstance prefixName
  showPrefixDec <- mkShowPrefixInstance prefixName prefixStr prettyStr
  factorDec <- mkPrefixFactorInstance prefixName fctr
  convFactorDec <- mkPrefixConvFactorInstance prefixName
  convUnitDec <- mkPrefixConvUnitInstance prefixName
  return $ [newtypeDec] ++ isUnitDec ++ showPrefixDec
        ++ factorDec ++ convFactorDec ++ convUnitDec





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
import Data.Units.Base.Prefix

------------------------------------ Units -------------------------------------

-- | List of derived class for a unit.
--
deriveList :: [Name]
deriveList =
  [ ''Eq
  , ''Ord
  , ''Num
  , ''Fractional
  , ''Floating
  , ''Real
  , ''RealFrac
  , ''RealFloat
  ]

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
-- instance Fractional a => ConvFactor Minute a where
--    factorFrom = 60
-- @
--
mkConvFactorFromInstance :: Quote m => Name -> Rational -> m [Dec]
mkConvFactorFromInstance unitName factor = [d|
  instance Fractional a => ConvFactor $(conT unitName) a where
    factorFrom = $(litE (RationalL factor))
  |]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvFactor Minute a where
--    factorTo = 60
-- @
--
mkConvFactorToInstance :: Quote m => Name -> Rational -> m [Dec]
mkConvFactorToInstance unitName factor = [d|
  instance Fractional a => ConvFactor $(conT unitName) a where
    factorTo = $(litE (RationalL factor))
  |]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvertibleUnit Minute a
-- @
--
mkDefaultSigConvertibleUnitInstance :: Quote m => Name -> m [Dec]
mkDefaultSigConvertibleUnitInstance unitName = [d|
  instance Fractional a => ConvertibleUnit $(conT unitName) a
  |]

-- | Make an instance of the form
--
-- @
-- instance Fractional a => ConvertibleUnit Meter a where
--    from = coerce
--    {-# INLINE from #-}
--    to = coerce
--    {-# INLINE to #-}
-- @
--
mkStdConvertibleUnitInstance :: Quote m => Name -> m [Dec]
mkStdConvertibleUnitInstance unitName = [d|
  instance Fractional a => ConvertibleUnit $(conT unitName) a where
    from = coerce
    {-# INLINE from #-}
    to = coerce
    {-# INLINE to #-}
  |]


-- | Make a unit that can be converted via a factor
--
-- [Usage:]
--
-- @
-- \$(mkUnit "Minute" "min" ''Time 60)
-- @
--
mkUnitFrom :: String -> String -> Name -> Rational -> Q [Dec]
mkUnitFrom unitStr prettyStr dimName factor = do
  let unitName = mkName unitStr
  newtypeDec <- mkUnitNewtype deriveListUnit unitName
  isUnitDec <- mkIsUnitInstance unitName dimName
  showUnitDec <- mkShowUnitInstance unitName unitStr prettyStr
  convFactorDec <- mkConvFactorFromInstance unitName factor
  convUnitDec <-
    if factor == 1 then
      mkStdConvertibleUnitInstance unitName
    else
      mkDefaultSigConvertibleUnitInstance unitName
  return $
    [newtypeDec] ++ isUnitDec ++ showUnitDec ++ convFactorDec ++ convUnitDec

-- | Make a unit that can be converted via a factor
--
-- [Usage:]
--
-- @
-- \$(mkUnitTo "Minute" "min" ''Time (1/60))
-- @
--
mkUnitTo :: String -> String -> Name -> Rational -> Q [Dec]
mkUnitTo unitStr prettyStr dimName factor = do
  let unitName = mkName unitStr
  newtypeDec <- mkUnitNewtype deriveListUnit unitName
  isUnitDec <- mkIsUnitInstance unitName dimName
  showUnitDec <- mkShowUnitInstance unitName unitStr prettyStr
  convFactorDec <- mkConvFactorToInstance unitName factor
  convUnitDec <-
    if factor == 1 then
      mkStdConvertibleUnitInstance unitName
    else
      mkDefaultSigConvertibleUnitInstance unitName
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

-- | Génère un newtype avec dérivations et deriving via
-- mkPrefixNewtype ''Milli
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
-- instance Fractional a => PrefixFactor Milli a where
--   prefixFactorTo = 1000
--   {-# INLINE prefixFactorTo #-}
--
mkPrefixFactorInstance :: Name -> Rational -> Q [Dec]
mkPrefixFactorInstance prefixName factor = [d|
  instance Fractional a => PrefixFactor $(conT prefixName) a where
    prefixFactorTo = $(litE (RationalL factor))
    {-# INLINE prefixFactorTo #-}
  |]

-- | Make an instance of the form
--
-- @
-- instance ConvFactor u a => ConvFactor (Milli u) a where
--   factorFrom = factorFrom @(MetaPrefix Milli u)
--   {-# INLINE factorFrom #-}
-- @
--
mkPrefixConvFactorFromInstance :: Name -> Q [Dec]
mkPrefixConvFactorFromInstance prefixName = [d|
  instance ConvFactor u a => ConvFactor ($(conT prefixName) u) a where
    factorFrom = factorFrom @(MetaPrefix $(conT prefixName) u)
    {-# INLINE factorFrom #-}
  |]

-- | Make an instance of the form
--
-- @
-- instance ConvFactor u a => ConvFactor (Milli u) a where
--   factorTo= factorTo @(MetaPrefix Milli u)
--   {-# INLINE factorTo #-}
-- @
--
mkPrefixConvFactorToInstance :: Name -> Q [Dec]
mkPrefixConvFactorToInstance prefixName = [d|
  instance ConvFactor u a => ConvFactor ($(conT prefixName) u) a where
    factorTo = factorTo @(MetaPrefix $(conT prefixName) u)
    {-# INLINE factorTo #-}
  |]

--

-- | Make an instance of the form
--
-- @
-- instance (ConvertibleUnit u a, Fractional a)
--   => ConvertibleUnit (Milli u) a where
--   from = prefixFrom
--   {-# INLINE from #-}
--   to = prefixTo
--   {-# INLINE to #-}
-- @
--
mkPrefixConvUnitInstance :: Name -> Q [Dec]
mkPrefixConvUnitInstance prefixName = [d|
  instance (ConvertibleUnit u a, Fractional a)
    => ConvertibleUnit ($(conT prefixName) u) a where
    from = prefixFrom
    {-# INLINE from #-}
    to = prefixTo
    {-# INLINE to #-}
  |]

-- | Make a unit prefix.
--
-- [Usage:]
--
-- @
-- \$(mkPrefixFrom "Kilo" "k" 1000)
-- @
--
mkPrefixFrom :: String -> String -> Rational -> Q [Dec]
mkPrefixFrom prefixStr prettyStr factor = do
  let prefixName = mkName prefixStr
  newtypeDec <- mkPrefixNewtype deriveList prefixName
  isUnitDec <- mkPrefixIsUnitInstance prefixName
  showPrefixDec <- mkShowPrefixInstance prefixName prefixStr prettyStr
  factorDec <- mkPrefixFactorInstance prefixName factor
  convFactorDec <- mkPrefixConvFactorFromInstance prefixName
  convUnitDec <- mkPrefixConvUnitInstance prefixName
  return $ [newtypeDec] ++ isUnitDec ++ showPrefixDec
        ++ factorDec ++ convFactorDec ++ convUnitDec

-- | Make a unit prefix.
--
-- [Usage:]
--
-- @
-- \$(mkPrefixTo "Kilo" "k" 1000)
-- @
--
mkPrefixTo :: String -> String -> Rational -> Q [Dec]
mkPrefixTo prefixStr prettyStr factor = do
  let prefixName = mkName prefixStr
  newtypeDec <- mkPrefixNewtype deriveList prefixName
  isUnitDec <- mkPrefixIsUnitInstance prefixName
  showPrefixDec <- mkShowPrefixInstance prefixName prefixStr prettyStr
  factorDec <- mkPrefixFactorInstance prefixName factor
  convFactorDec <- mkPrefixConvFactorToInstance prefixName
  convUnitDec <- mkPrefixConvUnitInstance prefixName
  return $ [newtypeDec] ++ isUnitDec ++ showPrefixDec
        ++ factorDec ++ convFactorDec ++ convUnitDec



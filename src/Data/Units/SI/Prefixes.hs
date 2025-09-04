{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Units.SI.Prefixes where

import Data.Units.Base


$(mkPrefixTo "Milli" "m" 1000)

$(mkPrefixFrom "Kilo" "k" 1000)

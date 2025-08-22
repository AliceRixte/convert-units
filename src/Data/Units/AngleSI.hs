
--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Units.AngleSI.Angle
-- Description :  SI with an angle dimension @A@
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- There is an ongoing debate about including Angle as a dimension of its own,
-- see for instance
--
-- * [On the dimension of angles and their
--   units](https://iopscience.iop.org/article/10.1088/1681-7575/ac7bc2/pdf) ,
--   Peter J Mohr /et all/, in Metrologia
-- * [Angles are inherently neither length ratios nor
--   dimensionless](https://iopscience.iop.org/article/10.1088/1681-7575/ab27d7/pdf),
--   Paul Quincey /et all/, in Metrologia
--
-- This module adds an Angle dimension to the SI system. To use dimensionless
-- angles, see "Data.Units.SI".
--
--------------------------------------------------------------------------------

module Data.Units.AngleSI
  ( module Data.Units.SI.Units
  , module Data.Units.SI.Prefixes
  , module Data.Units.SI.Derived
  , module Data.Units.AngleSI.Angle
  ) where

import Data.Units.SI.Units
import Data.Units.SI.Prefixes
import Data.Units.SI.Derived
import Data.Units.AngleSI.Angle
{-# LANGUAGE TemplateHaskell #-}
module Data.Units.NonStd.Area
  ( Perch(..)
  , Rood(..)
  , Acre(..)
  , SquareMile(..)
  ) where

import Data.Units.Base
import Data.Units.SI

-- TODO: add doc for each
-- XXX: what about those without abbreviation or symbol?
-- XXX: Do we just lowercase the name of the unit, e.g. Rood->rood?
$(mkUnit "Perch" "" ''Area $ (1200 / 3937)^2 * 1089 / 4)
$(mkUnit "Rood" "" ''Area $ (1200 / 3937)^2 * 10890)
$(mkUnit "Acre" "" ''Area $ (1200 / 3937)^2 * 43560)
$(mkUnit "SquareMile" "sq mi" ''Area $ (1200 / 3937)^2 * 27878400)

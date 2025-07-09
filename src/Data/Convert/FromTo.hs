{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Convert.FromTo
  ( module Data.Convert.FromTo
  ) where

import Data.Kind

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

class HasStandard q where
  type StandardOf q

type family StdEqErr q r where
  StdEqErr q r = If (StandardOf q == StandardOf r) (() :: Constraint)
    (TypeError (
        Text "Cannot convert "
        :<>: ShowType q
        :<>: Text " to  "
        :<>: ShowType r
        :<>: Text " because their standard target do not match."
        :$$: Text "Standard of "
        :<>: ShowType q
        :<>: Text " is: "
        :<>: ShowType (StandardOf q)
        :$$: Text "Standard of "
        :<>: ShowType q
        :<>: Text " is: "
        :<>: ShowType (StandardOf r)
    ))

type StdEq q r = (StdEqErr q r, StandardOf q ~ StandardOf r)


class HasStandard q => From q where
  from :: q -> StandardOf q

class HasStandard q => To q where
  to :: StandardOf q -> q

fromTo :: (StdEq q r, From q, To r) => q -> r
fromTo = to . from

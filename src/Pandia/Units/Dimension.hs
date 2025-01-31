{-# LANGUAGE NoStarIsType #-}

module Pandia.Units.Dimension
  ( module Pandia.Units.Dimension
  ) where

import Data.Proxy
import Data.Kind
import Data.Functor.Identity
import GHC.TypeLits

data Dimension l m t i th n j = Dimension l m t i th n j

type family DimMult (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                    (d':: Dimension Nat Nat Nat Nat Nat Nat Nat)
                    :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  DimMult ('Dimension l m t i th n j)
          ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

type family DimDiv (d :: Dimension Nat Nat Nat Nat Nat Nat Nat)
                   (d':: Dimension Nat Nat Nat Nat Nat Nat Nat)
                   :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  DimDiv ('Dimension l m t i th n j)
        ('Dimension l' m' t'  i' th' n' j') =
    'Dimension (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

type family DimPow (d :: Dimension Nat Nat Nat Nat Nat Nat Nat) (n :: Nat)
                   :: Dimension Nat Nat Nat Nat Nat Nat Nat where
  DimPow ('Dimension l m t i th n j) n' =
    'Dimension (n * n) (m * n') (t * n') (i * n') (th * n') (n * n') (j * n')
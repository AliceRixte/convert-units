module Data.Epsilon
  ( MkEpsilon (..)
  , aboutEqual
  , isApproxId
  , module Linear.Epsilon
  ) where

import Foreign.C.Types (CFloat, CDouble)
import Linear.Epsilon

-- | Provides a near-zero quantity
class MkEpsilon a where
  -- | A near-zero quantity
  epsilon :: a

-- | ε = 1e-6
instance MkEpsilon Float where
  epsilon = 1e-6

-- | ε = 1e-12
instance MkEpsilon Double where
  epsilon = 1e-12

-- | ε = 1e-6
instance MkEpsilon CFloat where
  epsilon = 1e-6

-- | ε = 1e-12
instance MkEpsilon CDouble where
  epsilon = 1e-12


-- | Equality up to epsilon
--
-- @'aboutEqual' a b@ is true iff @|a - b| < ε@
aboutEqual :: Epsilon a => a -> a -> Bool
aboutEqual a b = nearZero (a - b)

isApproxId :: Epsilon a => (a -> a) -> a -> Bool
isApproxId f a = aboutEqual (f a) a


{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Units.Base.Constructor
  ( module Data.Units.Base.Constructor
  , Unit
  ) where


import Data.Coerce
import Data.Proxy


import Data.Type.Int

import Data.Units.Base.Unit

----------------------------- Unit construction ------------------------------


-- | Multiply two quantities
--
(-*-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Num a)
 => u a -> v a -> (u -*- v) a
u -*- v = coerce (coerce u * coerce v :: a)
{-# INLINE (-*-) #-}


-- | Divide two quantities
--
(-/-) :: forall u v a. (Coercible a (u a), Coercible a (v a), Fractional a)
  => u a -> v a -> (u -/- v) a
u -/- v = coerce (coerce u / coerce v :: a)
{-# INLINE (-/-) #-}



type a -^+ b = a -^- Pos b
infix 8 -^+

type a -^~ b = a -^- Neg b
infix 8 -^~

-- | Raise a quantity to a power
--
(-^-) :: forall u n a. (Coercible a (u a), Num a)
  => u a -> Proxy n -> (u -^- n) a
u -^- _ = coerce u
{-# INLINE (-^-) #-}



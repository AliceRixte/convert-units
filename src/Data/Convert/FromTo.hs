{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Data.Convert.FromTo
  ( module Data.Convert.FromTo
  ) where

import Data.Coerce
import Control.Newtype
import GHC.TypeLits


class (Coercible u a, Coercible (Standard u) a) =>
  Dimensional u a | u -> a where
  factor :: a


type family Standard a

class From u where
  from :: u -> Standard u

instance {-# OVERLAPPABLE #-} (Fractional a, Dimensional u a)
  => From u where
  from u = coerce $ coerce u / factor @u

class To a where
  to :: Standard a -> a

instance {-# OVERLAPPABLE #-} (Num a, Dimensional u a)
  => To u where
  to u = coerce $ coerce u * factor @u

class (Standard a ~ Standard b) => FromTo a b where
  fromTo :: a -> b

instance {-# OVERLAPPABLE #-} (Standard a ~ Standard b, From a, To b)
  => FromTo a b where
  fromTo = to . from
  {-# INLINE fromTo #-}



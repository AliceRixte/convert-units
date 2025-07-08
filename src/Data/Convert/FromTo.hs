{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Data.Convert.FromTo
  ( module Data.Convert.FromTo
  ) where

import Data.Coerce


class (Coercible u a, Coercible (Standard u) a) =>
  Dimensional u a | u -> a where
  factor :: a


type family Standard a

class From u where
  from :: u -> Standard u

instance {-# OVERLAPPABLE #-} (Num a, Dimensional u a)
  => From u where
  from u = coerce $ coerce u * factor @u

class To a where
  to :: Standard a -> a

instance {-# OVERLAPPABLE #-} (Fractional a, Dimensional u a)
  => To u where
  to u = coerce $ coerce u / factor @u

convert :: (Standard a ~ Standard b, From a, To b) => a -> b
convert = to . from

convert' :: forall u v a.
  ( Dimensional (u a) a, Dimensional (v a) a
  , Standard (u a) ~ Standard (v a)
  , Fractional a
  )
  => u a -> v a
convert' u = coerce $ (coerce u :: a) * (factor @(u a) / factor @(v a))




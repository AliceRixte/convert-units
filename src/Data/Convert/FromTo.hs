module Data.Convert.FromTo
  ( module Data.Convert.FromTo
  ) where

type family Standard a

class From a where
  from :: a -> Standard a

class To a where
  to :: Standard a -> a

class Fractional a => Factor a where
  factor :: a
module Pandia.Units.Temperature
  ( module Pandia.Units.Temperature
  ) where

import Pandia.Units.Convert

kelvin :: a -> a
kelvin = id

kelvins :: a -> a
kelvins = id

class Celsius a where
  celsius :: a -> a

instance Fractional a => Celsius (From a) where
  celsius x = x - 273.15
  {-# INLINE celsius #-}

instance Fractional a => Celsius (To a) where
  celsius x = x + 273.15
  {-# INLINE celsius #-}

instance Celsius (Per (From a)) where
  celsius = id
  {-# INLINE celsius #-}

instance Celsius (Per (To a)) where
  celsius = id
  {-# INLINE celsius #-}
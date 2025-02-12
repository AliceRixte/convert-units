module Pandia.Units.NonStd.Angle
  ( module Pandia.Units.NonStd.Angle
  ) where

import Pandia.Units.Core



----------------------------------- Angle ------------------------------------

-- | Angle in degrees
--
newtype Degree a = Degree a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Degree where
  type DimOf syst Degree = DimA syst (Pos 1)

-- | Convertor for angle in degrees
--
degree :: ConvertorClass Degree cd p a => Convertor Degree cd p a
degree = convertor
{-# INLINE degree #-}

instance Floating a => ConvertorClass Degree 'ToDimSys 'False a where
  convertor _ x = x * pi / 180
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'ToDimSys 'True a where
  convertor _ _ = pi / 180
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'FromDimSys 'False a where
  convertor _ x = x * 180 / pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Degree 'FromDimSys 'True a where
  convertor _ _ = 180 / pi
  {-# INLINE convertor #-}


-- | Angle in complete turns (also called cycles or revolutions)
--
-- See https://en.wikipedia.org/wiki/Turn_(angle)
newtype Turn a = Turn a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Turn where
  type DimOf syst Turn = DimA syst (Pos 1)

-- | Convertor for angle in complete turns
--
turn :: ConvertorClass Turn cd p a => Convertor Turn cd p a
turn = convertor
{-# INLINE turn #-}

instance Floating a => ConvertorClass Turn 'ToDimSys 'False a where
  convertor _ x = x * 2 * pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'ToDimSys 'True a where
  convertor _ _ = 2 * pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'FromDimSys 'False a where
  convertor _ x = x / (2 * pi)
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Turn 'FromDimSys 'True a where
  convertor _ _ = 1 / (2 * pi)
  {-# INLINE convertor #-}


-- | Angle in gradians
--
-- See https://en.wikipedia.org/wiki/Gradian
newtype Gradian a = Gradian a
  deriving (Show, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat
          , Bounded, Enum, Semigroup, Monoid)

instance HasDim syst Gradian where
  type DimOf syst Gradian = DimA syst (Pos 1)

-- | Convertor for angle in gradians
--
gradian :: ConvertorClass Gradian cd p a => Convertor Gradian cd p a
gradian = convertor
{-# INLINE gradian #-}

instance Floating a => ConvertorClass Gradian 'ToDimSys 'False a where
  convertor _ x = x * pi / 200
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'ToDimSys 'True a where
  convertor _ _ = pi / 200
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'FromDimSys 'False a where
  convertor _ x = x * 200 / pi
  {-# INLINE convertor #-}

instance Floating a => ConvertorClass Gradian 'FromDimSys 'True a where
  convertor _ _ = 200 / pi
  {-# INLINE convertor #-}


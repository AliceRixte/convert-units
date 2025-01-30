module Pandia.Units.Combinators
  ( module Pandia.Units.Combinators
  ) where


-- per :: Fractional a => (a -> a) -> (a -> a)-> (a -> a)
-- per f g a = f a / g 1
-- infix 8 `per`

-- (.^) :: (a -> a) -> Int -> (a -> a)
-- (.^) f n  | n <= 0    = error "Exponent must be positive"
--           | n == 1    = f
--           | otherwise = f . (f .^ (n - 1))
-- {-# INLINE (.^) #-}
-- infixl 9 .^


module Data.Type.Int.Proxy
  ( zero
  , pos1
  , neg1
  , pos2
  , neg2
  , pos3
  , neg3
  , pos4
  , neg4
  )
  where

import Data.Proxy
import Data.Type.Int

zero :: Proxy Zero
zero = Proxy

pos1 :: Proxy (Pos 1)
pos1 = Proxy

neg1 :: Proxy (Neg 1)
neg1 = Proxy

pos2 :: Proxy (Pos 2)
pos2 = Proxy

neg2 :: Proxy (Neg 2)
neg2 = Proxy

pos3 :: Proxy (Pos 3)
pos3 = Proxy

neg3 :: Proxy (Neg 3)
neg3 = Proxy

pos4 :: Proxy (Pos 4)
pos4 = Proxy

neg4 :: Proxy (Neg 4)
neg4 = Proxy

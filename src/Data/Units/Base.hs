module Data.Units.Base
  ( -- ** Core modules
    module Data.Units.Base.System
  , module Data.Units.Base.Convert
  , module Data.Units.Base.Arithmetic
  , module Data.Units.Base.Prefix
  , module Data.Units.Base.TH
  -- ** Re-exported for Template Haskell to work out of the box
  -- on @import Data.Units.Base@
  , coerce
  -- ** Type level integers
  , module Data.Type.Int
  , module Data.Type.Int.Proxy
  -- ** Re-exported from GHC.TypeError
  , ErrorMessage (..)
  ) where

import GHC.TypeError (ErrorMessage(..))
import Data.Coerce (coerce)

import Data.Units.Base.System
import Data.Units.Base.Convert
import Data.Units.Base.Arithmetic
import Data.Units.Base.Prefix
import Data.Units.Base.TH
import Data.Type.Int
import Data.Type.Int.Proxy
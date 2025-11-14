module Data.Units.Core
  ( -- ** Core modules
    module Data.Units.Core.System
  , module Data.Units.Core.Convert
  , module Data.Units.Core.Arithmetic
  , module Data.Units.Core.Prefix
  , module Data.Units.Core.TH
  -- ** Re-exported for Template Haskell to work out of the box
  -- on @import Data.Units.Core@
  , coerce
  -- ** Type level integers
  , module Data.Type.Int
  -- ** Re-exported from GHC.TypeError
  , ErrorMessage (..)
  ) where

import GHC.TypeError (ErrorMessage(..))
import Data.Coerce (coerce)

import Data.Units.Core.System
import Data.Units.Core.Convert
import Data.Units.Core.Arithmetic
import Data.Units.Core.Prefix
import Data.Units.Core.TH
import Data.Type.Int
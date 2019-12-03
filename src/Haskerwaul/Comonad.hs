module Haskerwaul.Comonad
  ( module Haskerwaul.Comonad
  -- * extended modules
  , module Haskerwaul.Monad
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Monad

-- | https://ncatlab.org/nlab/show/comonad
type Comonad k ob = Monad (Op k) ob

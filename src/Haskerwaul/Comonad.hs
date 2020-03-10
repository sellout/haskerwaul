module Haskerwaul.Comonad
  ( module Haskerwaul.Comonad
  -- * extended modules
  , module Haskerwaul.Monad
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Monad

-- | [nLab](https://ncatlab.org/nlab/show/comonad)
type Comonad c ob = Monad (Op c) ob

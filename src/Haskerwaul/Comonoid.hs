module Haskerwaul.Comonoid
  ( module Haskerwaul.Comonoid
  -- * extended modules
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/comonoid)
type Comonoid c = Monoid (Op c)

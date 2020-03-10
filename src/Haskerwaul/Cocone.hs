module Haskerwaul.Cocone
  ( module Haskerwaul.Cocone
  -- * extended modules
  , module Haskerwaul.Cone
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Cone

-- | [nLab](https://ncatlab.org/nlab/show/cocone)
type Cocone k = Cone (Op k)

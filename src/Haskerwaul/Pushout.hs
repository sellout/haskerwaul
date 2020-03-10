module Haskerwaul.Pushout
  ( module Haskerwaul.Pushout
  -- * extended modules
  , module Haskerwaul.Pullback
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Pullback

-- | [nLab](https://ncatlab.org/nlab/show/pushout)
type Pushout c = Pullback (Op c)

type Coproduct c = Product (Op c)

-- coproduct :: CocartesianMonoidalCategory c => Coproduct c x y
-- coproduct = product

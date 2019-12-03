module Haskerwaul.Pushout
  ( module Haskerwaul.Pushout
  -- * extended modules
  , module Haskerwaul.Pullback
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Pullback

-- | https://ncatlab.org/nlab/show/pushout
type Pushout k = Pullback (Op k)

type Coproduct k = Product (Op k)

-- coproduct :: CocartesianCategory k => Coproduct k x y
-- coproduct = product

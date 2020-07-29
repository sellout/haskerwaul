module Haskerwaul.Category.Under
  ( module Haskerwaul.Category.Under
  -- * extended modules
  , module Haskerwaul.Category.Over
  ) where

import Haskerwaul.Category.Over
import Haskerwaul.Category.Opposite

-- | In our representation of an under (or coslice) category __x/c__, the
--   objects are /represented by/ the objects of the underlying category, but
--   the terms are still all morphisms.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/under+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Comma_category#Coslice_category)
type Under c = Over (Op c)

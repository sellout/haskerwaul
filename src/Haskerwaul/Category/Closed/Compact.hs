{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Compact where

import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Symmetric

-- | [nLab](https://ncatlab.org/nlab/show/compact+closed+category)
class (SymmetricMonoidalCategory c t, ClosedMonoidalCategory c t) =>
      CompactClosedCategory c t

-- -- | Compact closed categories are self-dual.
-- instance CompactClosedCategory c t => CompactClosedCategory (Opposite c) t

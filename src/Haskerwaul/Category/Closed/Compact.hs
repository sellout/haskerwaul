{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Compact where

import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Symmetric

-- | https://ncatlab.org/nlab/show/compact+closed+category
class (SymmetricMonoidalCategory k t, ClosedMonoidalCategory k t) =>
      CompactClosedCategory k t

-- -- | Compact closed categories are self-dual.
-- instance CompactClosedCategory k t => CompactClosedCategory (Opposite k) t

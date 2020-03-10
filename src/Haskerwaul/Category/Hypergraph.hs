module Haskerwaul.Category.Hypergraph
  ( module Haskerwaul.Category.Hypergraph
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Symmetric
  , module Haskerwaul.Monoid.Frobenius.Commutative.Special
  ) where

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Monoid.Frobenius.Commutative.Special
import Haskerwaul.Subcategory.Full

-- | [nLab](https://ncatlab.org/nlab/show/hypergraph+category)
type HypergraphCategory c t =
  SymmetricMonoidalCategory (FullSubcategory (SpecialCommutativeFrobeniusMonoid c t) c) t

-- -- | https://ncatlab.org/nlab/show/hypergraph+category#examples
-- instance Category c =>
--          SymmetricMonoidalCategory
--          (FullSubcategory (SpecialCommutativeFrobeniusMonoid (Cospan c) t) (Cospan c))
--          t

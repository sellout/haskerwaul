module Haskerwaul.Category.Hypergraph
  ( module Haskerwaul.Category.Hypergraph
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Symmetric
  , module Haskerwaul.Monoid.Frobenius.Commutative.Special
  ) where

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Monoid.Frobenius.Commutative.Special
import Haskerwaul.Subcategory.Full

type HypergraphCategory k t =
  SymmetricMonoidalCategory (FullSubcategory (SpecialCommutativeFrobeniusMonoid k t) k) t

-- -- | https://ncatlab.org/nlab/show/hypergraph+category#examples
-- instance Category k =>
--          SymmetricMonoidalCategory
--          (FullSubcategory (SpecialCommutativeFrobeniusMonoid (Cospan k) t) (Cospan k))
--          t

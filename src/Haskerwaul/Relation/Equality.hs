{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equality
  ( module Haskerwaul.Relation.Equality
  -- * extended modules
  , module Haskerwaul.Order.Partial
  , module Haskerwaul.Relation.Equivalence
  ) where

import Haskerwaul.Order.Partial
import Haskerwaul.Relation.Equivalence

-- | [nLab](https://ncatlab.org/nlab/show/equality#in_set_theory)
--
--  __NB__: Instances for this are automatically coalesced.
class (EquivalenceRelation c a, PartialOrder c a) => EqualityRelation c a

eq :: EqualityRelation c a => BinaryRelation c a a
eq = equiv

instance (EquivalenceRelation c a, PartialOrder c a) => EqualityRelation c a

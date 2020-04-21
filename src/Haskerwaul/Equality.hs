{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Equality
  ( module Haskerwaul.Equality
  -- * extended modules
  , module Haskerwaul.Order.Partial
  , module Haskerwaul.Relation.Equivalence
  ) where

import Haskerwaul.Order.Partial
import Haskerwaul.Relation.Equivalence

-- |
--  __NB__: Instances for this are automatically coalesced.
class (EquivalenceRelation c a, PartialOrder c a) => Equality c a

instance (EquivalenceRelation c a, PartialOrder c a) => Equality c a

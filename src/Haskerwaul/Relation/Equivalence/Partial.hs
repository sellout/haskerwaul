{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence.Partial
  ( module Haskerwaul.Relation.Equivalence.Partial
  -- * extended modules
  , module Haskerwaul.Relation.Homogeneous
  ) where

import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- |
-- = laws
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`equiv` x y ==> `equiv` y x@
--   [transitivity]: @`equiv` x y && `equiv` y z ==> `equiv` x z@
class HomogeneousRelation c a => PartialEquivalenceRelation c a

equiv :: PartialEquivalenceRelation c a => BinaryRelation c a a
equiv = rel

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Meet a)) =>
         PartialEquivalenceRelation c (Meet a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Join a)) =>
         PartialEquivalenceRelation c (Join a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Additive a)) =>
         PartialEquivalenceRelation c (Additive a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Multiplicative a)) =>
         PartialEquivalenceRelation c (Multiplicative a)

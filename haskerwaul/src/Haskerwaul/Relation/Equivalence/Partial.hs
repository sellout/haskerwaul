{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence.Partial
  ( module Haskerwaul.Relation.Equivalence.Partial,

    -- * extended modules
    module Haskerwaul.Relation.Homogeneous,
  )
where

import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Void (Void)
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
class (HomogeneousRelation' c a) => PartialEquivalenceRelation c a

equiv :: (PartialEquivalenceRelation c a) => BinaryRelation c a a
equiv = rel

instance PartialEquivalenceRelation (->) ()

instance PartialEquivalenceRelation (->) Void

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Const a b)) =>
  PartialEquivalenceRelation c (Const a b)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Identity a)) =>
  PartialEquivalenceRelation c (Identity a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Meet a)) =>
  PartialEquivalenceRelation c (Meet a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Join a)) =>
  PartialEquivalenceRelation c (Join a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Additive a)) =>
  PartialEquivalenceRelation c (Additive a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Multiplicative a)) =>
  PartialEquivalenceRelation c (Multiplicative a)

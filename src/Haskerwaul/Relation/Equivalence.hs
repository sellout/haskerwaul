{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence
  ( module Haskerwaul.Relation.Equivalence
  -- * extended modules
  , module Haskerwaul.Preorder
  , module Haskerwaul.Relation.Equivalence.Partial
  , module Haskerwaul.Relation.Tolerance
  ) where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice
import Haskerwaul.Object
import Haskerwaul.Preorder
import Haskerwaul.Semiring.Idempotent
import Haskerwaul.Relation.Equivalence.Partial
import Haskerwaul.Relation.Tolerance
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/equivalence+relation)
--
--   At minimum, an `EquivalenceRelation` /is/ a `Preorder`, where @`le` =
--  `equiv`@. However, more usefully, an `EquivalenceRelation` is congruent with
--   a `Preorder` if @`equiv` x y ==> `le` x y@.
--
--  __NB__: Instances for this are automatically coalesced.
--
-- = laws
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`equiv` x x = `true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`equiv` x y ==> `equiv` y x@
--   [transitivity]: @`equiv` x y && `equiv` y z ==> `equiv` x z@
class (Preorder c a, PartialEquivalenceRelation c a, ToleranceRelation c a) =>
      EquivalenceRelation c a where

-- | Every `Semilattice` has a canonical `Haskerwaul.Order.PartialOrder` as
--   @`le` (a, b) = `equiv` (`op` (a, b), b)@.
--
--  __FIXME__: How are these `Ob` constraints not implied by
--             @`ElementaryTopos` c@?
canonicalOrderFromSemilattice
  :: forall c a
   . (ElementaryTopos c, Ob c a, Ob c (Prod c a a), EquivalenceRelation c a, Semilattice c (Prod c) a)
  => BinaryRelation c a a
canonicalOrderFromSemilattice = equiv . first @c p op . to assoc . second p (diagonal @c)
  where
    p = Proxy :: Proxy c

-- | Every `IdempotentSemiring` has a canonical `Haskerwaul.Order.PartialOrder`
--   as the canonical `Haskerwaul.Order.PartialOrder` on its `Additive`
--  `Semilattice`. This also applies more generally to
--  `Haskerwaul.Dioid.Dioid`s, but there we don't have the benefit of an
--  `Additive` `Semilattice` to fall back on.
canonicalOrderFromIdempotentSemiring
  :: (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, IdempotentSemiring c (Prod c) a)
  => BinaryRelation c a a
canonicalOrderFromIdempotentSemiring =
  canonicalOrderFromSemilattice . bimap Add Add

-- | Every `Lattice` has a canonical `Haskerwaul.Order.PartialOrder` as the
--   canonical `Haskerwaul.Order.PartialOrder` on its `Join` `Semilattice`.
canonicalOrderFromLattice
  :: forall c a
   . (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Lattice c (Prod c) a)
  => BinaryRelation c a a
canonicalOrderFromLattice =
  equiv
  . bimap (getJoin . op) getJoin
  . to assoc
  . second p (diagonal @c)
  . bimap Join Join
  where
    p = Proxy :: Proxy c

instance (Preorder c a, PartialEquivalenceRelation c a, ToleranceRelation c a) =>
         EquivalenceRelation c a

-- __TODO__: Where can these instances live without being orphaned?
--
-- instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Semilattice c (Prod c) (Meet a)) =>
--          Preorder c (Meet a) where
--   le = canonicalOrderFromSemilattice
--
-- instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Semilattice c (Prod c) (Join a)) =>
--          Preorder c (Join a) where
--   le = canonicalOrderFromSemilattice

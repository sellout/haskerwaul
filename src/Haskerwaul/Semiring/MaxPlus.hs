module Haskerwaul.Semiring.MaxPlus
  ( module Haskerwaul.Semiring.MaxPlus
  -- * extended modules
  , module Haskerwaul.Semiring.Idempotent
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Lattice.Bounded
import Haskerwaul.Rig.Idempotent
import Haskerwaul.Semiring.Idempotent
import Haskerwaul.Hemiring

-- | [nLab](https://ncatlab.org/nlab/show/max-plus+algebra)
type MaxPlusSemiring c t a = IdempotentSemiring c t (Join a)

-- A MaxPlus semiring arises from the combination of a Hemiring and a
-- Lattice. Not sure I hit all the instances here. Need to get tests working ...

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         Magma c t (Additive (Join a)) where
  op = Add . op . bimap sum sum

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         Semigroup c t (Additive (Join a))

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         CommutativeMagma c t (Additive (Join a))

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         IdempotentMagma c t (Additive (Join a))

-- | With a `BoundedLattice`, we actually end up with an `IdempotentRig`.
instance (c ~ (->), MonoidalCategory c t, Hemiring c t a, BoundedLattice c t a) =>
         UnitalMagma c t (Additive (Join a)) where
  unit t = Add . unit t

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         Magma c t (Multiplicative (Join a)) where
  op = Multiply . Join . add . bimap (getJoin . product) (getJoin . product)

instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
         Semigroup c t (Multiplicative (Join a))

instance (c ~ (->), MonoidalCategory c t, Hemiring c t a, Lattice c t a) =>
         UnitalMagma c t (Multiplicative (Join a)) where
  unit t = Multiply . Join . zero t

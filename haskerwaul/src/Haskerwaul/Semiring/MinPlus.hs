{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Haskerwaul.Semiring.MinPlus
  ( module Haskerwaul.Semiring.MinPlus,

    -- * extended modules
    module Haskerwaul.Semiring.Idempotent,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Hemiring
import Haskerwaul.Lattice.Bounded
import Haskerwaul.Rig.Idempotent
import Haskerwaul.Semiring.Idempotent

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/min-plus+algebra)
type MinPlusSemiring c t a = IdempotentSemiring c t (Meet a)

-- A MinPlus semiring arises from the combination of a Hemiring and a
-- Lattice. Not sure I hit all the instances here. Need to get tests working ...

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  Magma c t (Additive (Meet a))
  where
  op = Add . op . bimap sum sum

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  FlexibleMagma c t (Additive (Meet a))

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  Semigroup c t (Additive (Meet a))

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  CommutativeMagma c t (Additive (Meet a))

-- instance (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
--          IdempotentMagma c t (Additive (Meet a))

-- | With a `BoundedLattice`, we actually end up with an `IdempotentRig`.
instance
  ( c ~ (->),
    -- NB: Needed in GHC 9.8 to avoid -Wloopy-superclass-solve
    MonoidalCategory' c t,
    MonoidalCategory c t,
    Hemiring c t a,
    BoundedLattice c t a
  ) =>
  UnitalMagma c t (Additive (Meet a))
  where
  unit t = Add . unit t

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  Magma c t (Multiplicative (Meet a))
  where
  op = Multiply . Meet . add . bimap (getMeet . product) (getMeet . product)

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  FlexibleMagma c t (Multiplicative (Meet a))

instance
  (c ~ (->), SemigroupalCategory c t, Hemiring c t a, Lattice c t a) =>
  Semigroup c t (Multiplicative (Meet a))

instance
  ( c ~ (->),
    -- NB: Needed in GHC 9.8 to avoid -Wloopy-superclass-solve
    MonoidalCategory' c t,
    MonoidalCategory c t,
    Hemiring c t a,
    Lattice c t a
  ) =>
  UnitalMagma c t (Multiplicative (Meet a))
  where
  unit t = Multiply . Meet . zero t

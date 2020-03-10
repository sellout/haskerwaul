{-# language UndecidableSuperClasses
           , UndecidableInstances #-}

module Haskerwaul.Hemiring
  ( module Haskerwaul.Hemiring
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Hemiring.Near
  ) where

import Haskerwaul.Magma
import Haskerwaul.Monoid.Commutative
import Haskerwaul.Hemiring.Near

-- | [Wikipedia](https://en.wikipedia.org/wiki/Semiring#Generalizations)
--
--  __NB__: While `Haskerwaul.Semiring.Pre.PreSemiring` and `Hemiring` are
--          usually synonymous, we distinguish them here because of the two
--          definitions of "semiring". Both are "semiring without multiplicative
--          identity", but we use `Haskerwaul.Semiring.Pre.PreSemiring` to be
--          relative to our `Haskerwaul.Semiring.Semiring`, while `Hemiring` is
--          relative to our `Haskerwaul.Rig.Rig` (which is called "semiring" in
--          other sources).
class (NearHemiring c t a, CommutativeMonoid c t (Additive a)) =>
      Hemiring c t a

instance (CommutativeMonoid c t (Additive a), Semigroup c t (Multiplicative a)) =>
         Hemiring c t a

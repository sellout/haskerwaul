{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Pre
  ( module Haskerwaul.Semiring.Pre
  -- * extended modules
  , module Haskerwaul.Semiring.Pre.Near
  ) where

import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semiring.Pre.Near

-- | [Wikipedia](https://en.wikipedia.org/wiki/Semiring#Generalizations)
--
--  __NB__: While `PreSemiring` and `Haskerwaul.Hemiring.Hemiring` are usually
--          synonymous, we distinguish them here because of the two definitions
--          of "semiring". Both are "semiring without multiplicative identity",
--          but we use `PreSemiring` to be relative to our
--         `Haskerwaul.Semiring.Semiring`, while `Haskerwaul.Hemiring.Hemiring`
--          is relative to our `Haskerwaul.Rig.Rig` (which is called "semiring"
--          in other sources).
class NearPreSemiring c t a => PreSemiring c t a

instance ( CommutativeSemigroup c t (Additive a)
         , Semigroup c t (Multiplicative a)) =>
         PreSemiring c t a

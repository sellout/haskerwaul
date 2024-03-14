{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring
  ( module Haskerwaul.Semiring
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semigroup.Commutative
  , module Haskerwaul.Semiring.Near
  ) where

import Haskerwaul.Magma
import Haskerwaul.Monoid
import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semiring.Near

-- | [nLab](https://ncatlab.org/nlab/show/rig)
--
--  __NB__: Instances for this are automatically coalesced.
class (NearSemiring c t a, CommutativeSemigroup c t (Additive a)) =>
      Semiring c t a

instance (CommutativeSemigroup c t (Additive a), Monoid c t (Multiplicative a)) =>
         Semiring c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semilattice
  ( module Haskerwaul.Semilattice
  -- * extended modules
  , module Haskerwaul.Semigroup.Commutative
  , module Haskerwaul.Semigroup.Idempotent
  ) where

import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semigroup.Idempotent

-- | [nLab](https://ncatlab.org/nlab/show/semilattice)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeSemigroup c t a, IdempotentSemigroup c t a) =>
      Semilattice c t a

instance (CommutativeSemigroup c t a, IdempotentSemigroup c t a) =>
         Semilattice c t a

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Idempotent
  ( module Haskerwaul.Semiring.Idempotent
  -- * extended modules
  , module Haskerwaul.Semilattice
  , module Haskerwaul.Semiring
  ) where

import Haskerwaul.Semilattice
import Haskerwaul.Semiring

-- | [nLab](https://ncatlab.org/nlab/show/idempotent+semiring)
-- 
--  __NB__: Instances for this are automatically coalesced.
class (Semiring c t a, Semilattice c t (Additive a)) => IdempotentSemiring c t a

instance (Semilattice c t (Additive a), Monoid c t (Multiplicative a)) =>
         IdempotentSemiring c t a

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Rig.Idempotent
  ( module Haskerwaul.Rig.Idempotent
  -- * extended modules
  , module Haskerwaul.Rig
  , module Haskerwaul.Semiring.Idempotent
  ) where

import Haskerwaul.Rig
import Haskerwaul.Semilattice.Bounded
import Haskerwaul.Semiring.Idempotent

-- | [nLab](https://ncatlab.org/nlab/show/idempotent+semiring)
-- 
--  __NB__: Instances for this are automatically coalesced.
class (IdempotentSemiring c t a, Rig c t a) => IdempotentRig c t a

instance (BoundedSemilattice c t (Additive a), Monoid c t (Multiplicative a)) =>
         IdempotentRig c t a

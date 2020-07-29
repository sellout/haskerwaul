{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Ring.Boolean
  ( module Haskerwaul.Ring.Boolean
  -- * extended modules
  , module Haskerwaul.Ring.Commutative
  , module Haskerwaul.Semilattice.Bounded
  ) where

import Haskerwaul.Ring.Commutative
import Haskerwaul.Semilattice.Bounded

-- |
-- = laws
--   [idempotency on `multiply`]: multiply x x == x
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Boolean+ring)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Boolean_ring)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeRing c t a, BoundedSemilattice c t (Multiplicative a)) =>
      BooleanRing c t a

instance (AbelianGroup c t (Additive a), BoundedSemilattice c t (Multiplicative a)) =>
         BooleanRing c t a

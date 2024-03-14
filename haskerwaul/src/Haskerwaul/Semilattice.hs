{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Semilattice
  ( module Haskerwaul.Semilattice,

    -- * extended modules
    module Haskerwaul.Band,
    module Haskerwaul.Semigroup.Commutative,
  )
where

import Haskerwaul.Band
import Haskerwaul.Semigroup.Commutative

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/semilattice)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeSemigroup c t a, Band c t a) => Semilattice c t a

instance (CommutativeSemigroup c t a, Band c t a) => Semilattice c t a

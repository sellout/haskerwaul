{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semilattice.Bounded
  ( module Haskerwaul.Semilattice.Bounded
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Semilattice
  ) where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Semilattice

-- | [nLab](https://ncatlab.org/nlab/show/semilattice#BoundedAndPseudo)
class (CommutativeMonoid c t a, Semilattice c t a) => BoundedSemilattice c t a

instance (CommutativeMonoid c t a, Semilattice c t a) =>
         BoundedSemilattice c t a

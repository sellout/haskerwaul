{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semilattice
  ( module Haskerwaul.Semilattice
  -- * extended modules
  , module Haskerwaul.Semigroup.Commutative
  ) where

import Haskerwaul.Semigroup.Commutative

-- | https://ncatlab.org/nlab/show/semilattice
class CommutativeSemigroup k t a => Semilattice k t a

instance Semilattice (->) (,) ()

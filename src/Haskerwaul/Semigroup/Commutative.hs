{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup.Commutative
  ( module Haskerwaul.Semigroup.Commutative
  -- * extended modules
  , module Haskerwaul.Magma.Commutative
  , module Haskerwaul.Semigroup
  ) where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Semigroup

class (CommutativeMagma k t a, Semigroup k t a) => CommutativeSemigroup k t a

instance (CommutativeMagma k t a, Semigroup k t a) => CommutativeSemigroup k t a

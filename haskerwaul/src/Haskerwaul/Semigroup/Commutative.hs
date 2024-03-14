{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup.Commutative
  ( module Haskerwaul.Semigroup.Commutative
  -- * extended modules
  , module Haskerwaul.Magma.Commutative
  , module Haskerwaul.Semigroup
  ) where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Semigroup

-- |
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeMagma c t a, Semigroup c t a) => CommutativeSemigroup c t a

instance (CommutativeMagma c t a, Semigroup c t a) => CommutativeSemigroup c t a

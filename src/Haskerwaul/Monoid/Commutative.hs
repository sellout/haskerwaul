{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Commutative
  ( module Haskerwaul.Monoid.Commutative
  -- * extended modules
  , module Haskerwaul.Magma.Commutative
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/commutative+monoid)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeMagma c t a,  Monoid c t a) => CommutativeMonoid c t a

instance (CommutativeMagma c t a,  Monoid c t a) => CommutativeMonoid c t a

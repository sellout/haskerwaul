{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Commutative
  ( module Haskerwaul.Monoid.Commutative
  -- * extended modules
  , module Haskerwaul.Magma.Commutative
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Monoid

class (CommutativeMagma k t a,  Monoid k t a) => CommutativeMonoid k t a

instance (CommutativeMagma k t a,  Monoid k t a) => CommutativeMonoid k t a

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring
  ( module Haskerwaul.Semiring
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semigroup.Commutative
  , module Haskerwaul.Semiring.Near
  ) where

import Haskerwaul.Magma
import Haskerwaul.Monoid
import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semiring.Near

class (NearSemiring k t a, CommutativeSemigroup k t (Additive a)) =>
      Semiring k t a

instance (CommutativeSemigroup k t (Additive a), Monoid k t (Multiplicative a)) =>
         Semiring k t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Commutative.Special
  ( module Haskerwaul.Monoid.Frobenius.Commutative.Special
  -- * extended modules
  , module Haskerwaul.Monoid.Frobenius.Commutative
  , module Haskerwaul.Monoid.Frobenius.Special
  ) where

import Haskerwaul.Monoid.Frobenius.Commutative
import Haskerwaul.Monoid.Frobenius.Special

class (SpecialFrobeniusMonoid k t a, CommutativeFrobeniusMonoid k t a) =>
      SpecialCommutativeFrobeniusMonoid k t a

instance (SpecialFrobeniusMonoid k t a, CommutativeFrobeniusMonoid k t a) =>
         SpecialCommutativeFrobeniusMonoid k t a

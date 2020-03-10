{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Commutative.Special
  ( module Haskerwaul.Monoid.Frobenius.Commutative.Special
  -- * extended modules
  , module Haskerwaul.Monoid.Frobenius.Commutative
  , module Haskerwaul.Monoid.Frobenius.Special
  ) where

import Haskerwaul.Monoid.Frobenius.Commutative
import Haskerwaul.Monoid.Frobenius.Special

class (SpecialFrobeniusMonoid c t a, CommutativeFrobeniusMonoid c t a) =>
      SpecialCommutativeFrobeniusMonoid c t a

instance (SpecialFrobeniusMonoid c t a, CommutativeFrobeniusMonoid c t a) =>
         SpecialCommutativeFrobeniusMonoid c t a

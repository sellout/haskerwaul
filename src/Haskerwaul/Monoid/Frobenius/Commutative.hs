{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Commutative
  ( module Haskerwaul.Monoid.Frobenius.Commutative
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Monoid.Frobenius
  ) where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Monoid.Frobenius

-- | https://ncatlab.org/nlab/show/Frobenius+algebra#commutative_frobenius_algebras
class (CommutativeMonoid k t a, FrobeniusMonoid k t a) =>
      CommutativeFrobeniusMonoid k t a

instance (CommutativeMonoid k t a, FrobeniusMonoid k t a) =>
         CommutativeFrobeniusMonoid k t a

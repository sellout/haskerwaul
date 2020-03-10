{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Commutative
  ( module Haskerwaul.Monoid.Frobenius.Commutative
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Monoid.Frobenius
  ) where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Monoid.Frobenius

-- | [nLab](https://ncatlab.org/nlab/show/Frobenius+algebra#commutative_frobenius_algebras)
class (CommutativeMonoid c t a, FrobeniusMonoid c t a) =>
      CommutativeFrobeniusMonoid c t a

instance (CommutativeMonoid c t a, FrobeniusMonoid c t a) =>
         CommutativeFrobeniusMonoid c t a

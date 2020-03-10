{-# language UndecidableSuperClasses #-}

module Haskerwaul.Group.Abelian
  ( module Haskerwaul.Group.Abelian
  -- * extended modules
  , module Haskerwaul.Group
  , module Haskerwaul.Monoid.Commutative
  ) where

import Haskerwaul.Group
import Haskerwaul.Monoid.Commutative

-- | [nLab](https://ncatlab.org/nlab/show/abelian+group)
class (CommutativeMonoid c t a, Group c t a) => AbelianGroup c t a

instance (CommutativeMonoid c t a, Group c t a) => AbelianGroup c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Group.Abelian
  ( module Haskerwaul.Group.Abelian
  -- * extended modules
  , module Haskerwaul.Group
  , module Haskerwaul.Monoid.Commutative
  ) where

import Haskerwaul.Group
import Haskerwaul.Monoid.Commutative

-- | https://ncatlab.org/nlab/show/abelian+group
class (CommutativeMonoid k t a, Group k t a) => AbelianGroup k t a

instance (CommutativeMonoid k t a, Group k t a) => AbelianGroup k t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid
  ( module Haskerwaul.Monoid
  -- * extended modules
  , module Haskerwaul.Magma.Unital
  , module Haskerwaul.Semigroup
  ) where

import Haskerwaul.Magma.Unital
import Haskerwaul.Semigroup

-- | https://ncatlab.org/nlab/show/monoid
class (UnitalMagma c t a, Semigroup c t a) => Monoid c t a

instance (UnitalMagma c t a, Semigroup c t a) => Monoid c t a

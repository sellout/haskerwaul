{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category
  ( module Haskerwaul.Category
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semigroupoid
  ) where

import Haskerwaul.Monoid
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/category)
class (Monoid (NaturalTransformation2 (->)) CProd c, Semigroupoid c) =>
      Category c

instance (Monoid (NaturalTransformation2 (->)) CProd c, Semigroupoid c) =>
         Category c

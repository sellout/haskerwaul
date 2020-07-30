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

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Category_(mathematics)
--  __NB__: Instances for this are automatically coalesced.
class (Monoid (NaturalTransformation2 (->)) Procompose c, Semigroupoid c) =>
      Category c

instance (Monoid (NaturalTransformation2 (->)) Procompose c, Semigroupoid c) =>
         Category c

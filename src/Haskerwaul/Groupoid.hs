{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Groupoid
  ( module Haskerwaul.Groupoid
  -- * extended modules
  , module Haskerwaul.Category
  , module Haskerwaul.Group
  ) where

import Haskerwaul.Category
import Haskerwaul.Group
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/groupoid)
--
--  __NB__: Instances for this are automatically coalesced.
class (Group (NaturalTransformation2 (->)) CProd a, Category a) => Groupoid a

instance (Group (NaturalTransformation2 (->)) CProd a, Category a) => Groupoid a

{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category
  ( module Haskerwaul.Category,

    -- * extended modules
    module Haskerwaul.Monoid,
    module Haskerwaul.Semicategory,
  )
where

import Haskerwaul.Monoid
import Haskerwaul.Semicategory
import Haskerwaul.Transformation.Dinatural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Category_(mathematics)
--
--  __NB__: Instances for this are automatically coalesced.
class (Monoid (DinaturalTransformation (->)) Procompose c) => Category c

instance (Monoid (DinaturalTransformation (->)) Procompose c) => Category c

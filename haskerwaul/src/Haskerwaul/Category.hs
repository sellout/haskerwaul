{-# LANGUAGE Safe #-}

module Haskerwaul.Category
  ( module Haskerwaul.Category,

    -- * extended modules
    module Haskerwaul.Magmoid.Unital,
    module Haskerwaul.Monoid,
    module Haskerwaul.Semicategory,
  )
where

import Haskerwaul.Magmoid.Unital
import Haskerwaul.Monoid
import Haskerwaul.Semicategory
import Haskerwaul.Transformation.Dinatural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Category_(mathematics)
type Category = Monoid (DinaturalTransformation (->)) Procompose

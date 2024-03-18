{-# LANGUAGE Safe #-}

module Haskerwaul.Category
  ( module Haskerwaul.Category,

    -- * extended modules
    module Haskerwaul.Magmoid.Unital,
    module Haskerwaul.Monoid,
    module Haskerwaul.Semicategory,
  )
where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Magmoid.Unital
import Haskerwaul.Monoid
import Haskerwaul.Semicategory

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Category_(mathematics)
type Category = HorizontalCategorification Monoid

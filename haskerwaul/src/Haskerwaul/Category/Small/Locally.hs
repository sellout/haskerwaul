{-# LANGUAGE Safe #-}

module Haskerwaul.Category.Small.Locally
  ( module Haskerwaul.Category.Small.Locally,

    -- * extended modules
    module Haskerwaul.Category,
  )
where

import Haskerwaul.Category

-- |
--  __NB__: Currently Haskerwaul /only/ deals in locally small categories, so
--          this is simply a synonym for `Category`. Once we abstract over the
--          enriching category (moving from @`Category` c@ to @`Category` v c@,
--          this should become a synonym for @`Category` (->)@.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/locally+small+category)
type LocallySmallCategory = Category

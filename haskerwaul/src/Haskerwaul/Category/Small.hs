{-# LANGUAGE Safe #-}

module Haskerwaul.Category.Small
  ( module Haskerwaul.Category.Small,

    -- * extended modules
    module Haskerwaul.Category.Small.Locally,
  )
where

import Haskerwaul.Category.Small.Locally

-- |
--
--  __NB__: Currently Haskerwaul /only/ deals in small categories, so
--          this is simply a synonym for `LocallySmallCategory`.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/small+category)
type SmallCategory = LocallySmallCategory

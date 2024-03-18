{-# LANGUAGE Safe #-}

module Haskerwaul.Groupoid
  ( module Haskerwaul.Groupoid,

    -- * extended modules
    module Haskerwaul.Category,
    module Haskerwaul.Group,
  )
where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Category
import Haskerwaul.Group

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/groupoid)
type Groupoid = HorizontalCategorification Group

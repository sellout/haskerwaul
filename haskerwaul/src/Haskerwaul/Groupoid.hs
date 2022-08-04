{-# LANGUAGE Safe #-}

module Haskerwaul.Groupoid
  ( module Haskerwaul.Groupoid,

    -- * extended modules
    module Haskerwaul.Category,
    module Haskerwaul.Group,
  )
where

import Haskerwaul.Category
import Haskerwaul.Group
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/groupoid)
type Groupoid = Group (DinaturalTransformation (->)) Procompose

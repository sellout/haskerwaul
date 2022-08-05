{-# LANGUAGE Safe #-}

module Haskerwaul.Category.Cancellative
  ( module Haskerwaul.Category.Cancellative,

    -- * extended modules
    module Haskerwaul.Category,
    module Haskerwaul.Monoid.Cancellative,
  )
where

import Haskerwaul.Category
import Haskerwaul.Monoid.Cancellative
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cancellative+category)
type CancellativeCategory =
  CancellativeMonoid (DinaturalTransformation (->)) Procompose

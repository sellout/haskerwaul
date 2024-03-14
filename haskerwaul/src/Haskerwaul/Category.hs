{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category
  ( module Haskerwaul.Category,

    -- * extended modules
    module Haskerwaul.Monoid,
    module Haskerwaul.Semigroupoid,
  )
where

import Haskerwaul.Monoid
import Haskerwaul.Semigroupoid
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

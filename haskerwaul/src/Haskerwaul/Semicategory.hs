{-# LANGUAGE Safe #-}

module Haskerwaul.Semicategory
  ( module Haskerwaul.Semicategory,

    -- * extended modules
    module Haskerwaul.Magmoid,
    module Haskerwaul.Semigroup,
  )
where

import Haskerwaul.Magmoid
import Haskerwaul.Semigroup
import Haskerwaul.Transformation.Dinatural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/semicategory)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Semigroupoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type Semicategory = Semigroup (DinaturalTransformation (->)) Procompose

{-# LANGUAGE Safe #-}

module Haskerwaul.Magmoid.Flexible
  ( module Haskerwaul.Magmoid.Flexible,

    -- * extended modules
    module Haskerwaul.Magma.Flexible,
    module Haskerwaul.Magmoid,
  )
where

import Haskerwaul.Magma.Flexible
import Haskerwaul.Magmoid
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/flexible+magmoid)
type FlexibleMagmoid = FlexibleMagma (DinaturalTransformation (->)) Procompose

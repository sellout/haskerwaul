{-# LANGUAGE Safe #-}

module Haskerwaul.Loopoid
  ( module Haskerwaul.Loopoid,

    -- * extended modules
    module Haskerwaul.Loop,
    module Haskerwaul.Magmoid.Unital,
    module Haskerwaul.Quasigroupoid,
  )
where

import Haskerwaul.Loop
import Haskerwaul.Magmoid.Unital
import Haskerwaul.Quasigroupoid
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/loopoid)
type Loopoid = Loop (DinaturalTransformation (->)) Procompose

{-# LANGUAGE Safe #-}

module Haskerwaul.Quasigroupoid.Associative
  ( module Haskerwaul.Quasigroupoid.Associative,

    -- * extended modules
    module Haskerwaul.Quasigroup.Associative,
    module Haskerwaul.Quasigroupoid,
  )
where

import Haskerwaul.Quasigroup.Associative
import Haskerwaul.Quasigroupoid
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/associative+quasigroupoid)
type AssociativeQuasigroupoid =
  AssociativeQuasigroup (DinaturalTransformation (->)) Procompose

{-# LANGUAGE Safe #-}

module Haskerwaul.Quasigroupoid.Associative
  ( module Haskerwaul.Quasigroupoid.Associative,

    -- * extended modules
    module Haskerwaul.Quasigroup.Associative,
    module Haskerwaul.Quasigroupoid,
  )
where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Quasigroup.Associative
import Haskerwaul.Quasigroupoid

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/associative+quasigroupoid)
type AssociativeQuasigroupoid = HorizontalCategorification AssociativeQuasigroup

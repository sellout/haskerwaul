{-# LANGUAGE Safe #-}

module Haskerwaul.Quasigroupoid
  ( module Haskerwaul.Quasigroupoid,

    -- * extended modules
    module Haskerwaul.Magmoid,
    module Haskerwaul.Quasigroup,
  )
where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Magmoid
import Haskerwaul.Quasigroup

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/quasigroupoid)
type Quasigroupoid = HorizontalCategorification Quasigroup

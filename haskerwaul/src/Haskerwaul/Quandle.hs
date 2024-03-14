{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quandle
  ( module Haskerwaul.Quandle,

    -- * extended modules
    module Haskerwaul.Quasigroup.Right,
    module Haskerwaul.Rack,
  )
where

import Haskerwaul.Quasigroup.Right
import Haskerwaul.Rack

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/quandle)
class (Rack c t a, RightQuasigroup c t a) => Quandle c t a

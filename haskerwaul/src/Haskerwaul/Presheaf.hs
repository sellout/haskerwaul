{-# LANGUAGE Safe #-}

module Haskerwaul.Presheaf
  ( module Haskerwaul.Presheaf,

    -- * extended modules
    module Haskerwaul.Functor,
  )
where

import Haskerwaul.Category.Opposite
import Haskerwaul.Functor

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/presheaf)
type Presheaf c = Functor (Opposite c) (->)

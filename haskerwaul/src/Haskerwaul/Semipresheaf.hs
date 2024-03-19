{-# LANGUAGE Safe #-}

module Haskerwaul.Semipresheaf
  ( module Haskerwaul.Semipresheaf,

    -- * extended modules
    module Haskerwaul.Semifunctor,
  )
where

import Haskerwaul.Category.Opposite
import Haskerwaul.Semifunctor

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/semipresheaf)
type Semipresheaf c = Semifunctor (Opposite c) (->)

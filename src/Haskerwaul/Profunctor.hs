{-# language UndecidableInstances #-}

module Haskerwaul.Profunctor
  ( module Haskerwaul.Profunctor
  -- * extended modules
  , module Haskerwaul.Bifunctor
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite

-- | [nLab](https://ncatlab.org/nlab/show/profunctor)
type Profunctor c d = Bifunctor (Op d) c (->)

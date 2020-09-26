{-# language UndecidableInstances #-}

module Haskerwaul.Profunctor
  ( module Haskerwaul.Profunctor
  -- * extended modules
  , module Haskerwaul.Bifunctor
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite

-- | Simply a synonym for certain bifunctors.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/profunctor)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Profunctor)
type Profunctor c d = Bifunctor (Op d) c (->)

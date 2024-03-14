{-# language UndecidableInstances #-}

module Haskerwaul.Profunctor
  ( module Haskerwaul.Profunctor
  -- * extended modules
  , module Haskerwaul.Bifunctor
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite
import Haskerwaul.Object

-- | Simply a synonym for certain bifunctors.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/profunctor)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Profunctor)
type Profunctor c d = Bifunctor (Opposite d) c (->)

-- | `bimap` specialized to `Profunctor`, to avoid dealing with `Opposite`.
promap :: (Profunctor c d f, Ob d a1, Ob d b1, Ob c a2, Ob c b2)
       => b1 `d` a1 -> a2 `c` b2 -> f a1 a2 -> f b1 b2
promap f = bimap (Opposite f)

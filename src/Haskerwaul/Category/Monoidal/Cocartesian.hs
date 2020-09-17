{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cocartesian
  ( module Haskerwaul.Category.Monoidal.Cocartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Opposite

-- | We provide an unwrapped version of `diagonal` as `codiagonal`, but the
--   [coprojections](https://ncatlab.org/nlab/show/coprojection) generally have
--   the same name as the
--   [projections](https://ncatlab.org/nlab/show/projection), so there's not
--   (yet) an unwrapped version of those.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cocartesian+monoidal+category)
type CocartesianMonoidalCategory c = CartesianMonoidalCategory (Opposite c)

type Coprod c = Prod (Opposite c)

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/codiagonal)
codiagonal :: (CocartesianMonodialCategory c, Ob c a) => Prod c a a `c` a
codiagonal = opposite diagonal

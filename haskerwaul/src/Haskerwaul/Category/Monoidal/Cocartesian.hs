{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cocartesian
  ( module Haskerwaul.Category.Monoidal.Cocartesian,
  )
where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Opposite
import Haskerwaul.Object

-- | We provide an unwrapped version of `diagonal` as `codiagonal`, but the
--   [coprojections](https://ncatlab.org/nlab/show/coprojection) generally have
--   the same name as the
--   [projections](https://ncatlab.org/nlab/show/projection), so there's not
--   (yet) an unwrapped version of those.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cocartesian+monoidal+category)
type CocartesianMonoidalCategory c = (CartesianMonoidalCategory (Opposite c))

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/coproduct)
type Coprod c = Prod (Opposite c)

inl :: (CocartesianMonoidalCategory c, Ob c a, Ob c b) => a `c` Coprod c a b
inl = opposite exl

inr :: (CocartesianMonoidalCategory c, Ob c a, Ob c b) => b `c` Coprod c a b
inr = opposite exr

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/codiagonal)
codiagonal :: (CocartesianMonoidalCategory c, Ob c a) => Coprod c a a `c` a
codiagonal = opposite diagonal

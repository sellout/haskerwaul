{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cocartesian
  ( module Haskerwaul.Category.Monoidal.Cocartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Opposite

-- | [nLab](https://ncatlab.org/nlab/show/cocartesian+monoidal+category)
type CocartesianMonoidalCategory c = CartesianMonoidalCategory (Op c)

type Coprod c = Prod (Op c)

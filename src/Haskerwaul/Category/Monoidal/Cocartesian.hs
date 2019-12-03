{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cocartesian
  ( module Haskerwaul.Category.Monoidal.Cocartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Opposite

-- | https://ncatlab.org/nlab/show/cocartesian+monoidal+category
type CocartesianMonoidalCategory k = CartesianMonoidalCategory (Op k)

type Coprod k = Prod (Op k)

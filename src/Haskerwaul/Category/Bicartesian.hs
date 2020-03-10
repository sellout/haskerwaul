{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Bicartesian
  ( module Haskerwaul.Category.Bicartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  , module Haskerwaul.Category.Monoidal.Cocartesian
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Cocartesian

-- | [nLab](https://ncatlab.org/nlab/show/bicartesian+category)
type BicartesianCategory c =
  (CartesianMonoidalCategory c, CocartesianMonoidalCategory c)

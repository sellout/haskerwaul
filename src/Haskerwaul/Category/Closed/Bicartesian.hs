module Haskerwaul.Category.Closed.Bicartesian
  ( module Haskerwaul.Category.Closed.Bicartesian
  -- * extended modules
  , module Haskerwaul.Category.Closed.Cartesian
  , module Haskerwaul.Category.Monoidal.Cocartesian
  ) where

import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Category.Monoidal.Cocartesian

-- | https://ncatlab.org/nlab/show/bicartesian+closed+category
type BicartesianClosedCategory k =
  (CartesianClosedCategory k, CocartesianMonoidalCategory k)

module Haskerwaul.Category.Distributive
  ( module Haskerwaul.Category.Distributive
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  , module Haskerwaul.Category.Monoidal.Distributive
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Distributive

-- | [nLab](https://ncatlab.org/nlab/show/distributive+category)
type DistributiveCategory c =
  (CartesianMonoidalCategory c, DistributiveMonoidalCategory c (Prod c))

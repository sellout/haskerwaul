module Haskerwaul.Category.Monoidal.Distributive
  ( module Haskerwaul.Category.Monoidal.Distributive
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cocartesian
  , module Haskerwaul.Category.Rig
  ) where

import Haskerwaul.Category.Monoidal.Cocartesian
import Haskerwaul.Category.Rig

-- | https://ncatlab.org/nlab/show/distributive+monoidal+category
type DistributiveMonoidalCategory k p =
  (CocartesianMonoidalCategory k, RigCategory k p (Coprod k))

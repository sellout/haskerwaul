module Haskerwaul.Category.CoKleisli
  ( module Haskerwaul.Category.CoKleisli
  -- * extended modules
  , module Haskerwaul.Category.Kleisli
  ) where

import Haskerwaul.Category.Kleisli
import Haskerwaul.Category.Opposite

-- | https://ncatlab.org/nlab/show/Kleisli+category+of+a+comonad
type CoKleisli k = Kleisli (Op k)

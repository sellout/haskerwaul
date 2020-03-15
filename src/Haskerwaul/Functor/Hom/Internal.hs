module Haskerwaul.Functor.Hom.Internal
  ( module Haskerwaul.Functor.Hom.Internal
  -- * extended modules
  , module Haskerwaul.Bifunctor
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite

-- | [nLab](https://ncatlab.org/nlab/show/internal+hom)
type InternalHomFunctor c = Bifunctor (Op c) c c

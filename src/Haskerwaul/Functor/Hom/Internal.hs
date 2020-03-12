module Haskerwaul.Functor.Hom.Internal
  ( module Haskerwaul.Functor.Hom.Internal
  -- * extended modules
  , module Haskerwaul.Bifunctor
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite

type InternalHomFunctor c = Bifunctor (Op c) c c

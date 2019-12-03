module Haskerwaul.Functor.Contravariant
  ( module Haskerwaul.Functor.Contravariant
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Functor

-- | https://ncatlab.org/nlab/show/contravariant+functor
type ContravariantFunctor c = Functor (Op c)

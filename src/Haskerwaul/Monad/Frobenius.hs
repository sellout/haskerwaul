module Haskerwaul.Monad.Frobenius
  ( module Haskerwaul.Monad.Frobenius
  , module Haskerwaul.Monoid.Frobenius
  ) where

import           Data.Functor.Compose (Compose)

import Haskerwaul.Monoid.Frobenius
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/Frobenius+monad)
type FrobeniusMonad c = FrobeniusMonoid (NaturalTransformation c) Compose

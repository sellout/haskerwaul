{-# language UndecidableInstances
           , UndecidableSuperClasses#-}

module Haskerwaul.Monad
  ( module Haskerwaul.Monad
  -- * extended modules
  , module Haskerwaul.Monoid
  ) where

import           Data.Functor.Compose (Compose)

import Haskerwaul.Endofunctor
import Haskerwaul.Monoid
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/monad)
class (Monoid (NaturalTransformation c) Compose m, Endofunctor c m) => Monad c m

instance (Monoid (NaturalTransformation c) Compose m, Endofunctor c m) =>
         Monad c m

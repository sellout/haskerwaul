{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Monoidal.Closed
  ( module Haskerwaul.Functor.Monoidal.Closed
  -- * extended modules
  , module Haskerwaul.Functor.Closed.Strong
  , module Haskerwaul.Functor.Monoidal.Strong
  ) where

import Haskerwaul.Functor.Closed.Strong
import Haskerwaul.Functor.Monoidal.Strong

-- | [nLab](https://ncatlab.org/nlab/show/closed+functor)
class (StrongClosedFunctor c d f, StrongMonoidalFunctor c ct d dt f) =>
      ClosedMonoidalFunctor c ct d dt f

instance (StrongClosedFunctor c d f, StrongMonoidalFunctor c ct d dt f) =>
         ClosedMonoidalFunctor c ct d dt f

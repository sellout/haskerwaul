{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Closed.Cartesian
  ( module Haskerwaul.Functor.Closed.Cartesian
  -- * extended modules
  , module Haskerwaul.Functor.Monoidal.Closed
  ) where

import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Functor.Monoidal.Closed

-- | [nLab](https://ncatlab.org/nlab/show/cartesian+closed+functor)
class ( CartesianClosedCategory c
      , CartesianClosedCategory d
      , ClosedMonoidalFunctor c (Prod c) d (Prod d) f) =>
      CartesianClosedFunctor c d f

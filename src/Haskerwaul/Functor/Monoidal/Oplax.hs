{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Monoidal.Oplax
  ( module Haskerwaul.Functor.Monoidal.Oplax
  -- * extended modules
  , module Haskerwaul.Functor.Monoidal.Lax
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Functor.Monoidal.Lax

-- | [nLab](https://ncatlab.org/nlab/show/oplax+monoidal+functor)
type OplaxMonoidalFunctor c ct d dt = LaxMonoidalFunctor (Op c) ct (Op d) dt

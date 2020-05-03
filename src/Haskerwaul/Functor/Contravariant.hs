{-# language UndecidableInstances #-}

module Haskerwaul.Functor.Contravariant
  ( module Haskerwaul.Functor.Contravariant
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/contravariant+functor)
--
--  __NB__: Instances for this are automatically coalesced.
class Functor (Op c) d f => ContravariantFunctor c d f

instance Functor (Op c) d f => ContravariantFunctor c d f

{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Functor.Diagonal where

import Haskerwaul.Bifunctor
import Haskerwaul.Category
import Haskerwaul.Functor

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/diagonal+functor)
newtype Diagonal t a = Diagonal {unDiagonal :: t a a}

instance
  (Semicategory c, Bifunctor c c (->) t) =>
  Semifunctor c (->) (Diagonal t)
  where
  map f (Diagonal t) = Diagonal (bimap f f t)

instance (Category c, Bifunctor c c (->) t) => Functor c (->) (Diagonal t)

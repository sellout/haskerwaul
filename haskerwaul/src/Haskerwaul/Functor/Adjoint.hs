{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Functor.Adjoint where

import Data.Proxy (Proxy)
import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/adjunction)
class (Functor c d l, Functor d c r) => AdjointFunctor c d l r where
  eta :: Proxy d -> a `c` r (l a)
  epsilon :: Proxy c -> l (r a) `d` a

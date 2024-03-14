{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Functor.Strong
  ( module Haskerwaul.Functor.Strong
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import Haskerwaul.Category.Monoidal
import Haskerwaul.Endofunctor
import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/tensorial+strength)
class (MonoidalCategory c t, Endofunctor c f) => StrongFunctor c t f where
  beta :: forall v w. t v (f w) `c` f (t v w)

-- | Every `Endofunctor` on __Set__ has tensorial strength.
instance Endofunctor (->) f => StrongFunctor (->) (,) f where
  beta (v, fw) = map (v, ) fw

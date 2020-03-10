{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Closed.Lax
  ( module Haskerwaul.Functor.Closed.Lax
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/closed+functor)
class (ClosedCategory c, ClosedCategory d, Functor c d f) =>
      LaxClosedFunctor c d f where
  fHat :: Proxy c -> f (Exp c x y) `d` Exp d (f x) (f y)

-- -- | Every `LaxMonoidalFunctor` between `ClosedMonoidalCategories` gives rise to
-- --   a `LaxClosedFunctor`.
-- instance ( ClosedMonoidalCategory c
--          , ClosedMonoidalCategory d
--          , LaxMonoidalFunctor c d f) =>
--          LaxClosedFunctor c d f where
--   fHat = 

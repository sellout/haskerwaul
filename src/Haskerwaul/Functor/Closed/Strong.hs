{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Closed.Strong
  ( module Haskerwaul.Functor.Closed.Strong
  -- * extended modules
  , module Haskerwaul.Functor.Closed.Lax
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Category.Closed
import Haskerwaul.Functor.Closed.Lax
import Haskerwaul.Isomorphism

-- | [nLab](https://ncatlab.org/nlab/show/closed+functor)
class (ClosedCategory c, ClosedCategory d, LaxClosedFunctor c d f) =>
      StrongClosedFunctor c d f where
  fHatOp :: Proxy c -> Exp d (f x) (f y) `d` f (Exp c x y)

fHatIso :: StrongClosedFunctor c d f
        => Proxy c -> Isomorphism d (f (Exp c x y)) (Exp d (f x) (f y))
fHatIso c = Iso (fHat c) (fHatOp c)

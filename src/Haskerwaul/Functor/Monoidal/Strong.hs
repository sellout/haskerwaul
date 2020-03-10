{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Monoidal.Strong
  ( module Haskerwaul.Functor.Monoidal.Strong
  -- * extended modules
  , module Haskerwaul.Functor.Monoidal.Lax
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Functor.Monoidal.Lax
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+functor)
class LaxMonoidalFunctor c ct d dt f => StrongMonoidalFunctor c ct d dt f where
  epsilonOp :: Proxy c -> Proxy ct -> Proxy dt -> f (Unit c ct) `d` Unit d dt
  muOp :: (Ob c x, Ob c y) => Proxy c -> f (ct x y) `d` dt (f x) (f y)

epsilonIso
  :: StrongMonoidalFunctor c ct d dt f
  => Proxy c
  -> Proxy ct
  -> Proxy dt
  -> Isomorphism d (Unit d dt) (f (Unit c ct))
epsilonIso c ct dt = Iso (epsilon c ct dt) (epsilonOp c ct dt)

muIso :: (StrongMonoidalFunctor c ct d dt f, Ob c x, Ob c y)
      => Proxy c -> Isomorphism d (dt (f x) (f y)) (f (ct x y))
muIso c = Iso (mu c) (muOp c)

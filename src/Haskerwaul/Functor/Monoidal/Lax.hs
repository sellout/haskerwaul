{-# language UndecidableSuperClasses #-}

module Haskerwaul.Functor.Monoidal.Lax
  ( module Haskerwaul.Functor.Monoidal.Lax
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import qualified Control.Applicative as Base
import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Void as Base

import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Endofunctor
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+functor)
class (MonoidalCategory c ct, MonoidalCategory d dt, Functor c d f) =>
      LaxMonoidalFunctor c ct d dt f where
  epsilon :: Proxy c -> Proxy ct -> Proxy dt -> Unit d dt `d` f (Unit c ct)
  mu :: (Ob c x, Ob c y) => Proxy c -> dt (f x) (f y) `d` f (ct x y)

instance Base.Applicative f => LaxMonoidalFunctor (->) (,) (->) (,) f where
  epsilon Proxy Proxy Proxy = Base.pure
  mu Proxy = from curry (Base.liftA2 (,))

instance Endofunctor (->) f => LaxMonoidalFunctor (->) Either (->) Either f where
  epsilon Proxy Proxy Proxy = Base.absurd
  mu Proxy = \case
    Left fx  -> map Left fx
    Right fy -> map Right fy

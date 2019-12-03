{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Duoid
  ( module Haskerwaul.Duoid
  -- * extended modules
  , module Haskerwaul.Monoid
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Monoid

newtype Diamond a = Diamond { getDiamond :: a }
newtype Star a = Star { getStar :: a }

class (Monoid k t (Diamond a), Monoid k t (Star a)) => Duoid k t a

instance (Monoid k t (Diamond a), Monoid k t (Star a)) => Duoid k t a

diamondU :: (k ~ (->), MonoidalCategory k t, Duoid k t a)
         => Proxy t -> Unit k t `k` a
diamondU t = getDiamond . unit t

starU :: (k ~ (->), MonoidalCategory k t, Duoid k t a)
      => Proxy t -> Unit k t `k` a
starU t = getStar . unit t

diamondT :: (k ~ (->), SemigroupalCategory k t, Duoid k t a, Bifunctor k k k t)
         => t a a `k` a
diamondT = getDiamond . op . bimap Diamond Diamond

starT :: (k ~ (->), SemigroupalCategory k t, Duoid k t a, Bifunctor k k k t)
      => t a a `k` a
starT = getStar . op . bimap Star Star

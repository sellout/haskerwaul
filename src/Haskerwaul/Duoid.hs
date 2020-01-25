{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Duoid
  ( module Haskerwaul.Duoid
  -- * extended modules
  , module Haskerwaul.Duoid.Components
  , module Haskerwaul.Monoid
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Duoid.Components
import Haskerwaul.Monoid
import Haskerwaul.Transformation.Natural

class (Monoid c t (Diamond a), Monoid c t (Star a)) => Duoid c t a

instance (Monoid c t (Diamond a), Monoid c t (Star a)) => Duoid c t a

diamondU :: (c ~ NaturalTransformation (->), MonoidalCategory c t, Duoid c t a)
         => Proxy t -> Unit c t `c` a
diamondU t = NT getDiamond . unit t

starU :: (c ~ NaturalTransformation (->), MonoidalCategory c t, Duoid c t a)
      => Proxy t -> Unit c t `c` a
starU t = NT getStar . unit t

diamondT :: (c ~ NaturalTransformation (->), SemigroupalCategory c t, Duoid c t a, Bifunctor c c c t)
         => t a a `c` a
diamondT = NT getDiamond . op . bimap (NT Diamond) (NT Diamond)

starT :: (c ~ NaturalTransformation (->), SemigroupalCategory c t, Duoid c t a, Bifunctor c c c t)
      => t a a `c` a
starT = NT getStar . op . bimap (NT Star) (NT Star)

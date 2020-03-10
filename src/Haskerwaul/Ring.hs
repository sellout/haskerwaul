{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Ring
  ( module Haskerwaul.Ring
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Rig
  , module Haskerwaul.Ring.Nonunital
  ) where

import Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Group
import Haskerwaul.Group.Abelian
import Haskerwaul.Rig
import Haskerwaul.Ring.Nonunital

-- | [nLab](https://ncatlab.org/nlab/show/ring)
class (AbelianGroup c t (Additive a), Rig c t a, NonunitalRing c t a) =>
      Ring c t a

instance (AbelianGroup c t (Additive a), Monoid c t (Multiplicative a)) =>
         Ring c t a

subtract :: (c ~ (->), MonoidalCategory c t, Ring c t a, Bifunctor c c c t)
         => t a a `c` a
subtract = sum . quotient . bimap Add Add

negate :: (c ~ (->), t ~ (,), MonoidalCategory c t, Ring c t a)
       => Proxy t -> a `c` a
negate t = sum . inverse t . Add

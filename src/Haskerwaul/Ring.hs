{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Ring
  ( module Haskerwaul.Ring
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Rig
  , module Haskerwaul.Ring.Nonunital
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Group
import Haskerwaul.Group.Abelian
import Haskerwaul.Rig
import Haskerwaul.Ring.Nonunital

-- | https://ncatlab.org/nlab/show/ring
class (AbelianGroup k t (Additive a), Rig k t a, NonunitalRing k t a) =>
      Ring k t a

instance (AbelianGroup k t (Additive a), Monoid k t (Multiplicative a)) =>
         Ring k t a

subtract :: (k ~ (->), MonoidalCategory k t, Ring k t a, Bifunctor k k k t)
         => t a a `k` a
subtract = sum . quotient . bimap Add Add

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
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Group
import Haskerwaul.Group.Abelian
import Haskerwaul.Rig
import Haskerwaul.Ring.Nonunital

-- | [nLab](https://ncatlab.org/nlab/show/ring)
class (AbelianGroup c t (Additive a), Rig c t a, NonunitalRing c t a) =>
      Ring c t a

instance (AbelianGroup c t (Additive a), Monoid c t (Multiplicative a)) =>
         Ring c t a

subtract :: (c ~ (->), SemigroupalCategory c t, Ring c t a)
         => t a a `c` a
subtract = sum . quotient . bimap Add Add

negate :: (c ~ (->), CartesianMonoidalCategory c, Ring c (Prod c) a) => a `c` a
negate = sum . inverse . Add

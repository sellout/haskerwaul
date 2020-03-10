{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Ring.Nonunital
  ( module Haskerwaul.Ring.Nonunital
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Hemiring
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Hemiring

-- | [nLab](https://ncatlab.org/nlab/show/nonunital+ring)
class (AbelianGroup c t (Additive a), Hemiring c t a) => NonunitalRing c t a

instance (AbelianGroup c t (Additive a), Semigroup c t (Multiplicative a)) =>
         NonunitalRing c t a

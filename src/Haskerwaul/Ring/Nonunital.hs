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

-- | https://ncatlab.org/nlab/show/nonunital+ring
class (AbelianGroup k t (Additive a), Hemiring k t a) => NonunitalRing k t a

instance (AbelianGroup k t (Additive a), Semigroup k t (Multiplicative a)) =>
         NonunitalRing k t a

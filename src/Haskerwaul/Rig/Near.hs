{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Rig.Near
  ( module Haskerwaul.Rig.Near
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semiring.Near
  ) where

import Haskerwaul.Monoid
import Haskerwaul.Semiring.Near

-- | https://ncatlab.org/nlab/show/near-ring
class (Monoid k t (Additive a), NearSemiring k t a) => NearRig k t a

instance (Monoid k t (Additive a), Monoid k t (Multiplicative a)) =>
         NearRig k t a

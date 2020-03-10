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

-- | [nLab](https://ncatlab.org/nlab/show/near-ring)
class (Monoid c t (Additive a), NearSemiring c t a) => NearRig c t a

instance (Monoid c t (Additive a), Monoid c t (Multiplicative a)) =>
         NearRig c t a

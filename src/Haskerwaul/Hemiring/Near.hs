{-# language UndecidableSuperClasses
           , UndecidableInstances #-}

module Haskerwaul.Hemiring.Near
  ( module Haskerwaul.Hemiring.Near
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semiring.Pre.Near
  ) where

import Haskerwaul.Monoid
import Haskerwaul.Semiring.Pre.Near

-- | https://ncatlab.org/nlab/show/near-ring
class (NearPreSemiring c t a, Monoid c t (Additive a)) =>
      NearHemiring c t a

instance (Monoid c t (Additive a), Semigroup c t (Multiplicative a)) =>
         NearHemiring c t a

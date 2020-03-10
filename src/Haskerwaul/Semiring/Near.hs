{-# language UndecidableSuperClasses
           , UndecidableInstances #-}

module Haskerwaul.Semiring.Near
  ( module Haskerwaul.Semiring.Near
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semiring.Pre.Near
  ) where

import Haskerwaul.Monoid
import Haskerwaul.Semiring.Pre.Near

-- | [nLab](https://ncatlab.org/nlab/show/near-ring)
class (NearPreSemiring c t a, Monoid c t (Multiplicative a)) =>
      NearSemiring c t a

instance (Semigroup c t (Additive a), Monoid c t (Multiplicative a)) =>
         NearSemiring c t a

{-# language UndecidableSuperClasses
           , UndecidableInstances #-}

module Haskerwaul.Semiring.Near
  ( module Haskerwaul.Semiring.Near
  -- * extended modules
  , module Haskerwaul.Monoid
  , module Haskerwaul.Semiring.Pre.Near
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Category.Monoidal
import Haskerwaul.Monoid
import Haskerwaul.Semiring.Pre.Near

-- | [nLab](https://ncatlab.org/nlab/show/near-ring)
--
--  __NB__: Instances for this are automatically coalesced.
class (NearPreSemiring c t a, Monoid c t (Multiplicative a)) =>
      NearSemiring c t a

-- | [nLab](https://ncatlab.org/nlab/show/one)
one :: (c ~ (->), MonoidalCategory c t, NearSemiring c t a)
    => Proxy t -> Unit c t `c` a
one t = product . unit t

instance (Semigroup c t (Additive a), Monoid c t (Multiplicative a)) =>
         NearSemiring c t a

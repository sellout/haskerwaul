{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Rig
  ( module Haskerwaul.Rig
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Rig.Near
  , module Haskerwaul.Semiring
  ) where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Rig.Near
import Haskerwaul.Semiring

-- | [nLab](https://ncatlab.org/nlab/show/rig)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeMonoid c t (Additive a), NearRig c t a, Semiring c t a) =>
      Rig c t a

instance (CommutativeMonoid c t (Additive a), Monoid c t (Multiplicative a)) =>
         Rig c t a

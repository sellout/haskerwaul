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

-- | https://ncatlab.org/nlab/show/rig
class (CommutativeMonoid k t (Additive a), NearRig k t a, Semiring k t a) =>
      Rig k t a

instance (CommutativeMonoid k t (Additive a), NearRig k t a, Semiring k t a) =>
         Rig k t a

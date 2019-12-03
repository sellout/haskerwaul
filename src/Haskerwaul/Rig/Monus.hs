{-# language UndecidableSuperClasses #-}

module Haskerwaul.Rig.Monus where

import           Numeric.Natural

import Haskerwaul.Monoid.Commutative.Monus
import Haskerwaul.Semiring

-- | https://ncatlab.org/nlab/show/rig
class (CommutativeMonoidMonus k t (Additive a), Semiring k t a) =>
      RigMonus k t a

instance RigMonus (->) (,) Natural

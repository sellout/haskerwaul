{-# language UndecidableSuperClasses #-}

module Haskerwaul.Rig.Monus where

import           Numeric.Natural

import Haskerwaul.Monoid.Commutative.Monus
import Haskerwaul.Semiring

-- | [nLab](https://ncatlab.org/nlab/show/rig)
class (CommutativeMonoidMonus c t (Additive a), Semiring c t a) =>
      RigMonus c t a

instance RigMonus (->) (,) Natural

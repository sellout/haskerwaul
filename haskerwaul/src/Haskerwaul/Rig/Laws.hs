{-# LANGUAGE RecordWildCards #-}

module Haskerwaul.Rig.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Monoid.Commutative.Laws
import Haskerwaul.Monoid.Laws
import Haskerwaul.Rig

data RigLaws c t a = RigLaws
  { commutativeMonoid :: CommutativeMonoidLaws c t (Additive a),
    monoid :: MonoidLaws c t (Multiplicative a)
  }

rigLaws :: (BraidedMonoidalCategory c t, Rig c t a) => RigLaws c t a
rigLaws =
  RigLaws
    { commutativeMonoid = commutativeMonoidLaws,
      monoid = monoidLaws
    }

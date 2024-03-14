{-# LANGUAGE Safe #-}

module Haskerwaul.Rig.Near.Laws where

import Haskerwaul.Category.Monoidal
import Haskerwaul.Monoid.Laws

data NearRigLaws c t a = NearRigLaws
  { additive :: MonoidLaws c t (Additive a),
    multiplicative :: MonoidLaws c t (Multiplicative a)
  }

rigLaws :: (MonoidalCategory c t, NearRig c t a) => NearRigLaws c t a
rigLaws =
  NearRigLaws
    { additive = monoidLaws,
      multiplicative = monoidLaws
    }

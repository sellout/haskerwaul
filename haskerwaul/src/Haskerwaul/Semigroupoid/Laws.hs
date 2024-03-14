module Haskerwaul.Semigroupoid.Laws where

import Haskerwaul.Semigroup.Laws
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Dinatural

type SemigroupoidLaws = SemigroupLaws (DinaturalTransformation (->)) Procompose

-- | Simply a specialization of `semigroupLaws` for when we're testing categories.
semigroupoidLaws :: (Semigroupoid c) => SemigroupoidLaws c
semigroupoidLaws = semigroupLaws

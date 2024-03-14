module Haskerwaul.Category.Laws where

import Haskerwaul.Category
import Haskerwaul.Monoid.Laws
import Haskerwaul.Transformation.Dinatural

type CategoryLaws = MonoidLaws (DinaturalTransformation (->)) Procompose

-- | Simply a specialization of `monoidLaws` for when we're testing categories.
categoryLaws :: (Category c) => CategoryLaws c
categoryLaws = monoidLaws

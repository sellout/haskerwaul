{-# LANGUAGE Safe #-}

module Haskerwaul.Semicategory.Laws where

import Haskerwaul.Semicategory
import Haskerwaul.Semigroup.Laws
import Haskerwaul.Transformation.Dinatural

type SemicategoryLaws = SemigroupLaws (DinaturalTransformation (->)) Procompose

-- | Simply a specialization of `semigroupLaws` for when we're testing categories.
semicategoryLaws :: (Semicategory c) => SemicategoryLaws c
semicategoryLaws = semigroupLaws

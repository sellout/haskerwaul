{-# LANGUAGE Safe #-}

module Haskerwaul.Semicategory.Laws where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Semicategory
import Haskerwaul.Semigroup.Laws

type SemicategoryLaws = HorizontalCategorification SemigroupLaws

-- | Simply a specialization of `semigroupLaws` for when we're testing categories.
semicategoryLaws :: (Semicategory c) => SemicategoryLaws c
semicategoryLaws = semigroupLaws

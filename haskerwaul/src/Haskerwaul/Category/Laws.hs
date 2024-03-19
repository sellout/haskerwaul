{-# LANGUAGE Safe #-}

module Haskerwaul.Category.Laws where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Category
import Haskerwaul.Monoid.Laws

type CategoryLaws = HorizontalCategorification MonoidLaws

-- | Simply a specialization of `monoidLaws` for when we're testing categories.
categoryLaws :: (Category c) => CategoryLaws c
categoryLaws = monoidLaws

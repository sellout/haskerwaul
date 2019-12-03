module Haskerwaul.Category.Functor
  ( module Haskerwaul.Category.Functor
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import Haskerwaul.Category
import Haskerwaul.Transformation.Natural

type FunctorCategory c d = Category (NaturalTransformation d)

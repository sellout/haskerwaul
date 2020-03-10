module Haskerwaul.Isomorphism.Natural
  ( module Haskerwaul.Isomorphism.Natural
  -- * extended modules
  , module Haskerwaul.Isomorphism
  , module Haskerwaul.Transformation.Natural
  ) where

import Haskerwaul.Isomorphism
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/natural+isomorphism)
type NaturalIsomorphism c = Isomorphism (NaturalTransformation c)

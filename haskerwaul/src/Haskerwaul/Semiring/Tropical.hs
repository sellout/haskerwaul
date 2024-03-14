module Haskerwaul.Semiring.Tropical
  ( module Haskerwaul.Semiring.Tropical
  -- * extended modules
  , module Haskerwaul.Semiring.MinPlus
  ) where

import Haskerwaul.Semiring.MinPlus

-- | [nLab](https://ncatlab.org/nlab/show/tropical+semiring)
type TropicalSemiring c t a = MinPlusSemiring c t a

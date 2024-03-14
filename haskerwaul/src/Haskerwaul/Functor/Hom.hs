module Haskerwaul.Functor.Hom
  ( module Haskerwaul.Functor.Hom
  -- * extended modules
  , module Haskerwaul.Profunctor
  ) where

import Haskerwaul.Profunctor

-- | [nLab](https://ncatlab.org/nlab/show/hom-functor)
type HomFunctor c = Profunctor c c

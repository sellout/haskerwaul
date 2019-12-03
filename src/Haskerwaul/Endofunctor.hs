module Haskerwaul.Endofunctor
  ( module Haskerwaul.Endofunctor
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import Haskerwaul.Functor

-- | https://ncatlab.org/nlab/show/endofunctor
type Endofunctor c = Functor c c

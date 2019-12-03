module Haskerwaul.Functor.Faithful
  ( module Haskerwaul.Functor.Faithful
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import Haskerwaul.Functor

-- | https://ncatlab.org/nlab/show/faithful+functor
class Functor c d f => FaithfulFunctor c d f

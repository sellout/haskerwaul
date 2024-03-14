module Haskerwaul.Functor.Faithful
  ( module Haskerwaul.Functor.Faithful
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import           Data.Constraint ((:-), Dict)
import           Data.Functor.Identity (Identity)

import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/faithful+functor)
class Functor c d f => FaithfulFunctor c d f

-- | `Dict` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor` between
--   the category of constraints and __Hask__.
instance FaithfulFunctor (:-) (->) Dict

-- | `Identity` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor`
--   (endofunctor, actually) in __Hask__.
instance FaithfulFunctor (->) (->) Identity



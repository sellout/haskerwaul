module Haskerwaul.Functor.Full
  ( module Haskerwaul.Functor.Full
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

import           Data.Constraint ((:-), Dict)
import           Data.Functor.Identity (Identity)

import Haskerwaul.Functor

-- | [nLab](https://ncatlab.org/nlab/show/full+functor)
class Functor c d f => FullFunctor c d f

-- | `Dict` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor` between
--   the category of constraints and __Hask__.
instance FullFunctor (:-) (->) Dict

-- | `Identity` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor`
--   (endofunctor, actually) in __Hask__.
instance FullFunctor (->) (->) Identity

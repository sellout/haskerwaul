module Haskerwaul.Functor.Faithful.Full
  ( module Haskerwaul.Functor.Faithful.Full
  -- * extended modules
  , module Haskerwaul.Functor.Faithful
  , module Haskerwaul.Functor.Full
  ) where

import           Data.Constraint ((:-), Dict, unmapDict)
import           Data.Functor.Identity (Identity(..))

import Haskerwaul.Functor.Faithful
import Haskerwaul.Functor.Full
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | [nLab](https://ncatlab.org/nlab/show/full+and+faithful+functor)
class (FullFunctor c d f, FaithfulFunctor c d f) =>
      FullFaithfulFunctor c d f where
  -- | This is truly a functor itself, but its signature doesn't match that
  --   required by `Functor`.
  --
  --  __NB__: This must form a lawful `Isomorphism` with `map`.
  unmap :: (Ob c a, Ob c b) => f a `d` f b -> a `c` b

-- | The isomorphism induced by a full & faithful functor.
fullFaithfulIso
  :: (FullFaithfulFunctor c d f, Ob c a, Ob c b)
  => Isomorphism (->) (a `c` b) (f a `d` f b)
fullFaithfulIso = Iso map unmap

-- | `Dict` is a `FullFaithfulFunctor` between the category of constraints and
--  __Hask__.
instance FullFaithfulFunctor (:-) (->) Dict where
  unmap = unmapDict

-- | `Identity` is a `FullFaithfulFunctor` (endofunctor, actually) in __Hask__.
instance FullFaithfulFunctor (->) (->) Identity where
  unmap f = runIdentity . f . Identity

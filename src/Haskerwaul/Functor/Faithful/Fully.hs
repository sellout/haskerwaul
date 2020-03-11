module Haskerwaul.Functor.Faithful.Full
  ( module Haskerwaul.Functor.Faithful.Full
  -- * extended modules
  , module Haskerwaul.Functor
  ) where

-- | [nLab](https://ncatlab.org/nlab/show/full+and+faithful+functor)
class (FullFunctor c d f, FaithfulFunctor c d f) =>
      FullFaithfulFunctor c d f where
  -- | Must form a lawful `Isomorphism` with `map`.
  unmap :: (Ob c a, Ob c b) => f a `d` f b -> a `c` b

fullFaithfulIso
  :: FullFaithfulFunctor c d f => Isomorphism (->) (a `c` b) (f a `d` f b)
fullFaithfulIso = Iso map unmap

-- | `Dict` is a `FullFaithfulFunctor` between the category of constraints and
--  __Hask__.
instance FullyFaithfulFunctor (:-) (->) Dict where
  unmap = unmapDict

{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Right
  ( module Haskerwaul.Extension.Kan.Right
  ) where

import           Data.Constraint ((\\))

import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Kan+extension)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Kan_extension)
data RightKanExtension c c' p f a = Ran (forall b. Ob c b => a `c'` p b -> f b)

instance (d ~ (->), Semigroupoid c', FOb (Ob c) (Ob c') p) =>
         Functor c' d (RightKanExtension c c' p f) where
  map f (Ran g) =
    Ran (\(h :: b `c'` p x) -> g (h . f) \\ inF @(Ob c) @(Ob c') @p @x)

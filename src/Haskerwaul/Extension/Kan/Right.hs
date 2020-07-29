{-# language UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Right
  ( module Haskerwaul.Extension.Kan.Right
  ) where

import Haskerwaul.Functor
import Haskerwaul.Semigroupoid

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Kan+extension)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Kan_extension)
newtype RightKanExtension c' p f a = Ran { runRan :: forall b. (a `c'` p b) -> f b }

instance (d ~ (->), Semigroupoid c') =>
         Functor c' d (RightKanExtension c' p f) where
  map f (Ran g) = Ran (g . (. f))

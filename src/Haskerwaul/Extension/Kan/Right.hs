{-# language UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Right
  ( module Haskerwaul.Extension.Kan.Right
  ) where

import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | [nLab](https://ncatlab.org/nlab/show/Kan+extension)
newtype RightKanExtension cp p f a = Ran { runRan :: forall b. (a `cp` p b) -> f b }

instance (d ~ (->), Semigroupoid cp, FOb (Ob cp) (Ob d) (RightKanExtension cp p f)) =>
         Functor cp d (RightKanExtension cp p f) where
  map f (Ran g) = Ran (g . (. f))

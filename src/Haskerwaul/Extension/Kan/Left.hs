{-# language GADTSyntax
           , UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Left
  ( module Haskerwaul.Extension.Kan.Left
  ) where

import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | [nLab](https://ncatlab.org/nlab/show/Kan+extension)
data LeftKanExtension cp p f a where
  Lan :: p b `cp` a -> f b -> LeftKanExtension cp p f a

instance (d ~ (->), Semigroupoid cp, FOb (Ob cp) (Ob d) (LeftKanExtension cp p f)) =>
         Functor cp d (LeftKanExtension cp p f) where
  map f (Lan g h) = Lan (f . g) h

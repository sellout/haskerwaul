{-# language GADTSyntax
           , TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Left
  ( module Haskerwaul.Extension.Kan.Left
  ) where

import           Data.Constraint ((\\))

import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- |
--  __NB__: This implementation, moreso than
--         `Haskerwaul.Extension.Kan.Right.RightKanExtension`, illustrates why
--          /D/ in the Kan extension
--
--             f
--           C -> D
--          p|  /`
--           C'
--
--          must be @(->)@. In the GADT, it's not possible for us to use an
--          arrow other than @->@ in the last position, and that is a morphism
--          in /D/.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Kan+extension)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Kan_extension)
data LeftKanExtension c c' p f a where
  Lan :: Ob c b => p b `c'` a -> f b -> LeftKanExtension c c' p f a

instance (d ~ (->), Semigroupoid c', FOb (Ob c) (Ob c') p) =>
         Functor c' d (LeftKanExtension c c' p f) where
  map f (Lan (g :: p x `c'` a) h) = Lan (f . g \\ inF @(Ob c) @(Ob c') @p @x) h

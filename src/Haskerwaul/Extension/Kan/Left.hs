{-# language GADTSyntax
           , UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Left
  ( module Haskerwaul.Extension.Kan.Left
  ) where

import Haskerwaul.Functor
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
data LeftKanExtension c' p f a where
  Lan :: p b `c'` a -> f b -> LeftKanExtension c' p f a

instance (d ~ (->), Semigroupoid c') =>
         Functor c' d (LeftKanExtension c' p f) where
  map f (Lan g h) = Lan (f . g) h

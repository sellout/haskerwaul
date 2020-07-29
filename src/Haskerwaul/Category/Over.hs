{-# language UndecidableInstances #-}

module Haskerwaul.Category.Over where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy (..))

import Haskerwaul.Object
import Haskerwaul.Topos.Elementary
import Haskerwaul.Transformation.Natural

-- | In our representation of an over (or slice) category __c/x__, the objects
--   are /represented by/ the objects of the underlying category, but the terms
--   are still all morphisms.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/over+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Comma_category#Slice_category)
newtype Over (c :: ok -> ok -> *) (x :: ok) a b = Over (a `c` x -> b `c` x)

type instance Ob (Over c x) = Ob c

instance Semigroupoid c => Magma (NaturalTransformation2 (->)) CProd (Over c x) where
  op = NT2 (\(CProd (Over f) (Over g)) -> Over (f . g))

instance (Semigroupoid c, Ob c x) => Semigroup (NaturalTransformation2 (->)) CProd (Over c x)

instance (Category c, Ob c x) => UnitalMagma (NaturalTransformation2 (->)) CProd (Over c x) where
  unit Proxy = NT2 (\Refl -> Over id)

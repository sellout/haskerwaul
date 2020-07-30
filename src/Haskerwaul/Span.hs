{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Span where

import           Data.Constraint ((\\))
import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Pullback
import Haskerwaul.Transformation.Natural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/span)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Span_(category_theory))
data Span c x y = forall s. Span (s `c` x) (s `c` y)

type instance Ob (Span c) = Ob c

-- These instances are fixed to @`Ob` c `~` `All`@ because I can't figure out
-- how to set the `Ob` constraint on the existential.

instance (Ob c ~ All, CartesianMonoidalCategory c) =>
         Magma (NaturalTransformation2 (->)) Procompose (Span c) where
  op =
    NT2
    (\(Procompose (Span b c) (Span a b')) ->
        let pb = cartesianPullbackSquare b' b in Span (a . lx pb) (c . ly pb)
    )

instance (Ob c ~ All, CartesianMonoidalCategory c) =>
         Semigroup (NaturalTransformation2 (->)) Procompose (Span c)

instance MonoidalCategory c t => MonoidalCategory' (Span c) t where
  type Unit (Span c) t = Unit c t

instance (Ob c ~ All, CartesianMonoidalCategory c) =>
         UnitalMagma (NaturalTransformation2 (->)) Procompose (Span c) where
  unit :: Proxy Procompose -> NaturalTransformation2 (->) (:~:) (Span c)
  unit Proxy = NT2 (\Refl -> Span id id)

instance (SemigroupalCategory c t, Magma c t a) => Magma (Span c) t a where
  op = Span id op \\ inT @(Ob c) @t @a @a

instance (SemigroupalCategory c t, Semigroup c t a) => Semigroup (Span c) t a

instance (MonoidalCategory c t, UnitalMagma c t a) =>
         UnitalMagma (Span c) t a where
  unit t = Span id (unit t)

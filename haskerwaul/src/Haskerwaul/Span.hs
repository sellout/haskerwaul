{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Span where

import Data.Constraint ((\\))
import Data.Constraint.Deferrable ((:~:) (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Pullback
import Haskerwaul.Transformation.Dinatural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/span)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Span_(category_theory))
data Span c x y = forall s. (Ob c s) => Span (s `c` x) (s `c` y)

type instance Ob (Span c) = Ob c

-- These instances are fixed to @`Ob` c `~` `All`@ because I can't figure out
-- how to set the `Ob` constraint on the existential.

instance
  (Ob c ~ All, CartesianMonoidalCategory c) =>
  Magma (DinaturalTransformation (->)) Procompose (Span c)
  where
  op =
    DT
      ( \(Procompose (Span b c) (Span a b')) ->
          let pb = cartesianPullbackSquare b' b in Span (a . lx pb) (c . ly pb)
      )

instance
  (Ob c ~ All, CartesianMonoidalCategory c) =>
  Semigroup (DinaturalTransformation (->)) Procompose (Span c)

instance (MonoidalCategory c t) => MonoidalCategory' (Span c) t where
  type Unit (Span c) t = Unit c t

instance
  (Ob c ~ All, CartesianMonoidalCategory c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (Span c)
  where
  unit :: Proxy Procompose -> DinaturalTransformation (->) (:~:) (Span c)
  unit Proxy = DT (\Refl -> Span id id)

instance (SemigroupalCategory c t, Magma c t a) => Magma (Span c) t a where
  op = Span id op \\ inT @(Ob c) @t @a @a

instance (SemigroupalCategory c t, Semigroup c t a) => Semigroup (Span c) t a

instance
  (MonoidalCategory c t, UnitalMagma c t a) =>
  UnitalMagma (Span c) t a
  where
  unit t = Span id (unit t)

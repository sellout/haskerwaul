{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Right where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

rightDistributiveLaw
  :: forall c a. (CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Prod c a a `c` a -> Law c EqualityRelation (Prod c (Prod c a a) a) a
rightDistributiveLaw multiply' add' =
  Law
  (multiply' . first p add')
  -- __TODO__: This rewrite seems overcomplicated.
  (add'
   . bimap multiply' multiply'
   . to assoc
   . second p (braid @c)
   . from assoc
   . first p (from (assoc @c))
   . to assoc
   . second @c p (diagonal @c))
  \\ inT @(Ob c) @(Prod c) @(Prod c a a) @a
  \\ inT @(Ob c) @(Prod c) @a @(Prod c a a)
  \\ inT @(Ob c) @(Prod c) @a @a
  where
    p = Proxy :: Proxy c

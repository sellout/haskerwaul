{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Left where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

leftDistributiveLaw
  :: forall c a. (CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Prod c a a `c` a -> Law c EqualityRelation (Prod c a (Prod c a a)) a
leftDistributiveLaw multiply add =
  Law
  (multiply . second p add)
  -- __TODO__: This rewrite seems overcomplicated.
  (add
   . bimap multiply multiply
   . from assoc
   . first p (braid @c)
   . to assoc
   . second p (to (assoc @c))
   . from assoc
   . first @c p (diagonal @c))
  \\ inT @(Ob c) @(Prod c) @(Prod c a a) @a
  \\ inT @(Ob c) @(Prod c) @a @(Prod c a a)
  \\ inT @(Ob c) @(Prod c) @a @a
  where
    p = Proxy :: Proxy c

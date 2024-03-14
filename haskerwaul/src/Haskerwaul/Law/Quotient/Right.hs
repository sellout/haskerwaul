{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Law.Quotient.Right where

import Data.Constraint ((\\))
import Data.Proxy (Proxy (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- | @y = x (x \ y)@
rq1Law ::
  forall c a.
  (CartesianMonoidalCategory c, Ob c a) =>
  Prod c a a `c` a ->
  Prod c a a `c` a ->
  Law c EqualityRelation (Prod c a a) a
rq1Law op' rightQuotient' =
  Law exr (op' . second p rightQuotient' . from assoc . first p (diagonal @_ @c))
    \\ inT @(Ob c) @(Prod c) @a @a
  where
    p = Proxy :: Proxy c

-- | @y = x \ (x y)@
rq2Law ::
  forall c a.
  (CartesianMonoidalCategory c, Ob c a) =>
  Prod c a a `c` a ->
  Prod c a a `c` a ->
  Law c EqualityRelation (Prod c a a) a
rq2Law op' rightQuotient' =
  Law exr (rightQuotient' . second p op' . from assoc . first p (diagonal @_ @c))
    \\ inT @(Ob c) @(Prod c) @a @a
  where
    p = Proxy :: Proxy c

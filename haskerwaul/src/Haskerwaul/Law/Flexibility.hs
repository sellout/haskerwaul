{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Law.Flexibility where

import Data.Constraint ((\\))
import Data.Proxy (Proxy (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- | [nLab](https://ncatlab.org/nlab/show/alternative+algebra)
flexibility ::
  forall c a.
  (CartesianMonoidalCategory c, Ob c a) =>
  Prod c a a `c` a ->
  Law c EqualityRelation (Prod c a a) a
flexibility op' =
  Law
    ( op'
        . first p op'
        . to (assoc . second @_ @(Isomorphism c) i braid . reverse assoc)
        . first @c p diagonal
    )
    (op' . second p (op' . to braid) . from assoc . first @c p diagonal)
    \\ inT @(Ob c) @(Prod c) @a @a
  where
    i = Proxy :: Proxy (Isomorphism c)
    p = Proxy :: Proxy c

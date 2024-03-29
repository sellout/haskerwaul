{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Law.Asymmetry where

import Data.Constraint ((\\))
import Data.Proxy (Proxy (..))
import Haskerwaul.Algebra.Boolean
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/asymmetric+relation)
asymmetry ::
  forall c a.
  (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), Ob c a) =>
  BinaryRelation c a a ->
  Law c EqualityRelation (Prod c a a) (Class c)
asymmetry op' =
  Law
    (true . (!))
    ( implies
        . bimap op' (complement (Proxy :: Proxy (Prod c)) . op' . to braid)
        . diagonal
    )
    \\ inT @(Ob c) @(Prod c) @a @a

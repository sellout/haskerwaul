{-# language TypeApplications #-}

module Haskerwaul.Law.Asymmetry where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Algebra.Boolean
import Haskerwaul.Bifunctor
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/asymmetric+relation)
asymmetry
  :: forall c a. (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), Ob c a)
  => BinaryRelation c a a -> Law c (Prod c a a) (Class c)
asymmetry op' =
  Law
  (true . (!))
  (implies . bimap op' (complement (Proxy :: Proxy (Prod c)) . op' . braid) . diagonal)
  \\ inT @(Ob c) @(Prod c) @a @a

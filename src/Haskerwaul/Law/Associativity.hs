{-# language TypeApplications #-}

module Haskerwaul.Law.Associativity where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- | [nLab](https://ncatlab.org/nlab/show/associativity)
associativity
  :: forall c t a. (SemigroupalCategory c t, Ob c a)
  => t a a `c` a -> Law c EqualityRelation (t (t a a) a) a
associativity op' =
  Law (op' . first p op') (op' . second p op' . from assoc)
  \\ inT @(Ob c) @t @(t a a) @a
  \\ inT @(Ob c) @t @a @a
  where
    p = Proxy :: Proxy c

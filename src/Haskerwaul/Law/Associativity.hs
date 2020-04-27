module Haskerwaul.Law.Associativity where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/associativity)
associativity :: forall c t a
                . (Ob c (t a a), SemigroupalCategory c t, Ob c a)
               => t a a `c` a -> Law c (t (t a a) a) a
associativity op' = Law (op' . first p op') (op' . second p op' . from assoc)
  where
    p = Proxy :: Proxy c

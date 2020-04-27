{-# language TypeApplications #-}

module Haskerwaul.Law.Flexibility where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/alternative+algebra)
flexibility :: forall c a
             . (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
            => Prod c a a `c` a -> Law c (Prod c a a) a
flexibility op' =
  Law
  (op'
   . first p op'
   . to assoc
   . second @_ @c p braid
   . from assoc
   . first @c p diagonal)
  (op' . second p (op' . braid) . from assoc . first @c p diagonal)
  where
    p = Proxy :: Proxy c

{-# language TypeApplications #-}

module Haskerwaul.Law.Alternativity.Right where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/alternative+algebra)
rightAlternativity :: forall c a
                    . (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
                   => Prod c a a `c` a -> Law c (Prod c a a) a
rightAlternativity op' =
  Law
  (op' . first p op' . to assoc . second @_ @c p diagonal)
  (op' . second p (op' . diagonal))
  where
    p = Proxy :: Proxy c

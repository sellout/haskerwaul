{-# language TypeApplications #-}

module Haskerwaul.Law.Alternativity.Left where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/alternative+algebra)
leftAlternativity :: forall c a
                   . (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
                  => Prod c a a `c` a -> Law c (Prod c a a) a
leftAlternativity op' =
  Law
  (op' . first p (op' . diagonal))
  (op' . second p op' . from assoc . first @c p diagonal)
  where
    p = Proxy :: Proxy c

{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Right where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

rightDistributiveLaw
  :: forall c a
   . ( Ob c (Prod c a a), Ob c (Prod c (Prod c a a) a), Ob c (Prod c a (Prod c a a))
     , CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c (Prod c a a) a) a
rightDistributiveLaw multiply' add' =
  Law
    (multiply' . first p add')
    -- __TODO__: This rewrite seems overcomplicated.
    (add'
      . bimap multiply' multiply'
      . to assoc
      . second p (braid @c)
      . from assoc
      . first p (from (assoc @c))
      . to assoc
      . second @c p (diagonal @c))
  where
    p = Proxy :: Proxy c

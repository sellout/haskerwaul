{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Left where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

leftDistributiveLaw
  :: forall c a
   . ( Ob c (Prod c a a), Ob c (Prod c a (Prod c a a)), Ob c (Prod c (Prod c a a) a)
     , CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c a (Prod c a a)) a
leftDistributiveLaw multiply add =
  Law
    (multiply . second p add)
    -- __TODO__: This rewrite seems overcomplicated.
    (add
      . bimap multiply multiply
      . from assoc
      . first p (braid @c)
      . to assoc
      . second p (to (assoc @c))
      . from assoc
      . first @c p (diagonal @c))
  where
    p = Proxy :: Proxy c

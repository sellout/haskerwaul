{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Left where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Semiring.Pre.Near

leftDistributiveLaw
  :: forall c a
   . ( c ~ (->)
     , CartesianMonoidalCategory c
     , Semigroup c (Prod c) (Additive a)
     , Semigroup c (Prod c) (Multiplicative a))
  => Law c (Prod c a (Prod c a a)) a
leftDistributiveLaw =
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
      . first @c p (duplicate @c))
  where
    p = Proxy :: Proxy c

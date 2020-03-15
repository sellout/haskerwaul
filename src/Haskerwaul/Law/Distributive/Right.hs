{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Right where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Semiring.Pre.Near

rightDistributiveLaw
  :: forall c a
   . ( c ~ (->)
     , CartesianMonoidalCategory c
     , Semigroup c (Prod c) (Additive a)
     , Semigroup c (Prod c) (Multiplicative a))
  => Law c (Prod c (Prod c a a) a) a
rightDistributiveLaw =
  Law
    (multiply . first p add)
    -- __TODO__: This rewrite seems overcomplicated.
    (add
      . bimap multiply multiply
      . to assoc
      . second p (braid @c)
      . from assoc
      . first p (from (assoc @c))
      . to assoc
      . second @c p (duplicate @c))
  where
    p = Proxy :: Proxy c

{-# language TypeApplications #-}

module Haskerwaul.Law.Quotient.Right where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | @y = x (x \ y)@
rq1Law :: forall c a. (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
       => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c a a) a
rq1Law op' rightQuotient' =
  Law exr (op' . second p rightQuotient' . from assoc . first p (diagonal @c))
  where
    p = Proxy :: Proxy c

-- | @y = x \ (x y)@
rq2Law :: forall c a. (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
       => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c a a) a
rq2Law op' rightQuotient' =
  Law exr (rightQuotient' . second p op' . from assoc . first p (diagonal @c))
  where
    p = Proxy :: Proxy c

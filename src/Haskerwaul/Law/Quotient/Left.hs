{-# language TypeApplications #-}

module Haskerwaul.Law.Quotient.Left where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | @x = (x / y) y@
lq1Law :: forall c a. (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
       => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c a a) a
lq1Law op' leftQuotient' =
  Law exr (op' . first p leftQuotient' . to assoc . second p (diagonal @c))
  where
    p = Proxy :: Proxy c

-- | @x = (x y) / y@
lq2Law :: forall c a. (Ob c (Prod c a a), CartesianMonoidalCategory c, Ob c a)
       => Prod c a a `c` a -> Prod c a a `c` a -> Law c (Prod c a a) a
lq2Law op' leftQuotient' =
  Law exr (leftQuotient' . first p op' . to assoc . second p (diagonal @c))
  where
    p = Proxy :: Proxy c

module Haskerwaul.Law.Idempotency where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/idempotent)
idempotency :: (CartesianMonoidalCategory c, Ob c a)
            => Prod c a a `c` a -> Law c a a
idempotency op' = Law id (op' . diagonal)

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Skewfield
  ( module Haskerwaul.Skewfield
  -- * extended modules
  , module Haskerwaul.Group
  , module Haskerwaul.Ring
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Group
import Haskerwaul.Ring

-- | [nLab](https://ncatlab.org/nlab/show/skewfield)
class (Group c t (Multiplicative a), Ring c t a) => Skewfield c t a

--   FIXME: The `divide` operation introduced here is partial.
divide :: (c ~ (->), MonoidalCategory c t, Skewfield c t a)
       => t a a `c` a
divide = product . quotient . bimap Multiply Multiply

reciprocal :: (c ~ (->), CartesianMonoidalCategory c, Skewfield c (Prod c) a)
           => a `c` a
reciprocal = product . inverse . Multiply

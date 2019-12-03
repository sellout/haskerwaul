{-# language UndecidableSuperClasses #-}

module Haskerwaul.Skewfield
  ( module Haskerwaul.Skewfield
  -- * extended modules
  , module Haskerwaul.Group
  , module Haskerwaul.Ring
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Group
import Haskerwaul.Ring

-- | https://ncatlab.org/nlab/show/skewfield
class (Group k t (Multiplicative a), Ring k t a) => Skewfield k t a

--   FIXME: The `divide` operation introduced here is partial.
divide :: (k ~ (->), MonoidalCategory k t, Skewfield k t a, Bifunctor k k k t)
       => t a a `k` a
divide = product . quotient . bimap Multiply Multiply

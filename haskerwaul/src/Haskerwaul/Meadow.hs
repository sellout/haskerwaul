{-# language UndecidableSuperClasses #-}

module Haskerwaul.Meadow
  ( module Haskerwaul.Meadow
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Ring.Commutative
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Ring.Commutative

-- | [Meadows as a New Theme in the Theory of Rings and Fields](https://meadowsite.wordpress.com/)
--
-- = laws
--   [reflection]: reciprocal (reciprocal x) == x
--   [restricted inverse]: x * (x * reciprocal x) == x
class (AbelianGroup c t (Multiplicative a), CommutativeRing c t a) =>
      Meadow c t a

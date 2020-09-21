{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Compact
  ( module Haskerwaul.Category.Closed.Compact
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Closed
  , module Haskerwaul.Category.Monoidal.Traced
  ) where

import           Data.Kind (Type)

import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Traced
import Haskerwaul.Category.Opposite

-- | This must be symmetrical. I.e., if @`DualOb` c a ~ b@, then @`DualOb` c b ~ a@.
type family DualOb (c :: ok -> ok -> Type) a

type ProperDual c a = DualOb c (DualOb c a) ~ a

-- | [nLab](https://ncatlab.org/nlab/show/compact+closed+category)
class (TracedMonoidalCategory c t, ClosedMonoidalCategory c t) =>
      CompactClosedCategory c t where
  comUnit :: ProperDual c a => Unit c t `c` t (DualOb c a) a
  comCounit :: ProperDual c a => t a (DualOb c a) `c` Unit c t

-- -- | Compact closed categories are self-dual.
-- instance CompactClosedCategory c t => CompactClosedCategory (Opposite c) t

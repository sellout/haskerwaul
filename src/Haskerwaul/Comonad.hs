{-# language UndecidableInstances #-}

module Haskerwaul.Comonad
  ( module Haskerwaul.Comonad
  -- * extended modules
  , module Haskerwaul.Monad
  ) where

import Haskerwaul.Semigroupoid
import Haskerwaul.Category.Opposite
import Haskerwaul.Monad

-- | [nLab](https://ncatlab.org/nlab/show/comonad)
type Comonad' c ob = Monad' (Op c) ob

-- | See `Monad` for an explanation as to why both `Comonad'` and `Comonad`.
--
--  __NB__: Instances for this are automatically coalesced.
class Monad (Opposite c) w => Comonad c w where
  copure :: w a `c` a
  copure = opposite pure
  duplicate :: w a `c` w (w a)
  duplicate = opposite flatten

instance Monad (Opposite c) a => Comonad c a

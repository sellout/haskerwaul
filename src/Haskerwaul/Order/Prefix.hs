{-# language UndecidableSuperClasses #-}

module Haskerwaul.Order.Prefix
  ( module Haskerwaul.Order.Prefix
  -- * extended modules
  , module Haskerwaul.Order.Partial
  ) where

import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Order.Partial

-- | [nLab](https://ncatlab.org/nlab/show/prefix+order)
--
-- = laws
--   [downward totality]: @`le` (x, z) && `le` (y, z) ==> `le` (x, y) || `le` (y, x)@
class PartialOrder c a => PrefixOrder c a

instance PrefixOrder (->) Natural

instance PrefixOrder (->) Int

instance PrefixOrder (->) Int8

instance PrefixOrder (->) Int16

instance PrefixOrder (->) Int32

instance PrefixOrder (->) Int64

instance PrefixOrder (->) Integer

instance PrefixOrder (->) Word

instance PrefixOrder (->) Word8

instance PrefixOrder (->) Word16

instance PrefixOrder (->) Word32

instance PrefixOrder (->) Word64

instance PrefixOrder (->) Float

instance PrefixOrder (->) Double

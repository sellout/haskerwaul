{-# language UndecidableSuperClasses #-}

module Haskerwaul.Order.Prefix
  ( module Haskerwaul.Order.Prefix
  -- * extended modules
  , module Haskerwaul.Order.Partial
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Negation
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Partial

-- | [nLab](https://ncatlab.org/nlab/show/prefix+order)
--
-- = laws
--   [downward totality]: @`le` (x, z) && `le` (y, z) ==> `le` (x, y) || `le` (y, x)@
class PartialOrder c a => PrefixOrder c a

instance PrefixOrder (->) (Negate ())

instance PrefixOrder (->) (Canonical Bool)

instance PrefixOrder (->) (Canonical Natural)

instance PrefixOrder (->) (Canonical Int)

instance PrefixOrder (->) (Canonical Int8)

instance PrefixOrder (->) (Canonical Int16)

instance PrefixOrder (->) (Canonical Int32)

instance PrefixOrder (->) (Canonical Int64)

instance PrefixOrder (->) (Canonical Integer)

instance PrefixOrder (->) (Canonical Word)

instance PrefixOrder (->) (Canonical Word8)

instance PrefixOrder (->) (Canonical Word16)

instance PrefixOrder (->) (Canonical Word32)

instance PrefixOrder (->) (Canonical Word64)

instance PrefixOrder (->) (Canonical Float)

instance PrefixOrder (->) (Canonical Double)

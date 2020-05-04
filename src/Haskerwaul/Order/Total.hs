{-# language UndecidableSuperClasses #-}

module Haskerwaul.Order.Total
  ( module Haskerwaul.Order.Total
  -- * extended modules
  , module Haskerwaul.Order.Prefix
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Proxy (Proxy(..))
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Algebra.Boolean
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Prefix
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/total+order)
--
-- = laws
--   [totality]: @`le` (x, y) || `le` (y, x) == `true` ()@
class PrefixOrder c a => TotalOrder c a

ge :: (ElementaryTopos c, TotalOrder c a) => BinaryRelation c a a
ge = le . braid

gt :: forall c a. (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), TotalOrder c a)
   => BinaryRelation c a a
gt = complement (Proxy :: Proxy (Prod c)) . le

lt :: (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), TotalOrder c a)
   => BinaryRelation c a a
lt = gt . braid

instance TotalOrder (->) (Canonical Bool)

instance TotalOrder (->) (Canonical Natural)

instance TotalOrder (->) (Canonical Int)

instance TotalOrder (->) (Canonical Int8)

instance TotalOrder (->) (Canonical Int16)

instance TotalOrder (->) (Canonical Int32)

instance TotalOrder (->) (Canonical Int64)

instance TotalOrder (->) (Canonical Integer)

instance TotalOrder (->) (Canonical Word)

instance TotalOrder (->) (Canonical Word8)

instance TotalOrder (->) (Canonical Word16)

instance TotalOrder (->) (Canonical Word32)

instance TotalOrder (->) (Canonical Word64)

instance TotalOrder (->) (Canonical Float)

instance TotalOrder (->) (Canonical Double)

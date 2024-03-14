{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Order.Total
  ( module Haskerwaul.Order.Total,

    -- * extended modules
    module Haskerwaul.Order.Prefix,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import qualified GHC.Real as Base
import Haskerwaul.Isomorphism
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Prefix
import Haskerwaul.Topos.Elementary
import Numeric.Natural (Natural)
import Prelude (Integer)

-- | [nLab](https://ncatlab.org/nlab/show/total+order)
--
-- = laws
--   [totality]: @`le` (x, y) || `le` (y, x) == `true` ()@
class (PrefixOrder c a) => TotalOrder c a

ge :: (ElementaryTopos c, TotalOrder c a) => BinaryRelation c a a
ge = le . to braid

instance TotalOrder (->) ()

instance TotalOrder (->) Void

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

instance (Base.Integral a) => TotalOrder (->) (Canonical (Ratio a))

-- * operators

(>=) :: (ElementaryTopos c, TotalOrder c a) => a `c` Exp c a (Class c)
(>=) = curry ge

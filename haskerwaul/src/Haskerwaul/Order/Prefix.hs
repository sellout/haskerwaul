{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Order.Prefix
  ( module Haskerwaul.Order.Prefix,

    -- * extended modules
    module Haskerwaul.Order.Partial,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import qualified GHC.Real as Base
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Partial
import Numeric.Natural (Natural)
import Prelude (Double, Float, Integer)

-- | [nLab](https://ncatlab.org/nlab/show/prefix+order)
--
-- = laws
--
-- - [downward totality]: @`le` (x, z) && `le` (y, z) ==> `le` (x, y) || `le` (y, x)@
class (PartialOrder c a) => PrefixOrder c a

instance PrefixOrder (->) ()

instance PrefixOrder (->) Void

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

instance (Base.Integral a) => PrefixOrder (->) (Canonical (Ratio a))

-- | `Float` doesn't have a stronger ordering than this because of NaNs. NaNs
--   are incomparable to anything.
--
--  __NB__: There are tradeoffs between this approach and one that fits NaNs
--          between -Inf +Inf (e.g., the other approach allows for a
--         `Haskerwaul.Lattice.Bounded.BoundedLattice` instance). We should
--          probably provide distinct @newtype@ wrappers so we can offer both.
instance PrefixOrder (->) (Canonical Float)

-- | `Double` doesn't have a stronger ordering than this because of NaNs. NaNs
--   are incomparable to anything.
--
--  __NB__: There are tradeoffs between this approach and one that fits NaNs
--          between -Inf +Inf (e.g., the other approach allows for a
--         `Haskerwaul.Lattice.Bounded.BoundedLattice` instance). We should
--          probably provide distinct @newtype@ wrappers so we can offer both.
instance PrefixOrder (->) (Canonical Double)

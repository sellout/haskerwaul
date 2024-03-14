{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Band
  ( module Haskerwaul.Band,

    -- * extended modules
    module Haskerwaul.Semigroup,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Lattice.Components
import Haskerwaul.Semigroup
import Numeric.Natural (Natural)
import Prelude (Integer)

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/band)
class (Semigroup c t a) => Band c t a

instance Band (->) (,) (Join Bool)

instance Band (->) (,) (Meet Bool)

instance Band (->) (,) (Join Int)

instance Band (->) (,) (Meet Int)

instance Band (->) (,) (Join Int8)

instance Band (->) (,) (Meet Int8)

instance Band (->) (,) (Join Int16)

instance Band (->) (,) (Meet Int16)

instance Band (->) (,) (Join Int32)

instance Band (->) (,) (Meet Int32)

instance Band (->) (,) (Join Int64)

instance Band (->) (,) (Meet Int64)

instance Band (->) (,) (Join Integer)

instance Band (->) (,) (Meet Integer)

instance Band (->) (,) (Join Natural)

instance Band (->) (,) (Meet Natural)

instance Band (->) (,) ()

instance Band (->) (,) (Join ())

instance Band (->) (,) (Meet ())

instance Band (->) (,) (Join Word)

instance Band (->) (,) (Meet Word)

instance Band (->) (,) (Join Word8)

instance Band (->) (,) (Meet Word8)

instance Band (->) (,) (Join Word16)

instance Band (->) (,) (Meet Word16)

instance Band (->) (,) (Join Word32)

instance Band (->) (,) (Meet Word32)

instance Band (->) (,) (Join Word64)

instance Band (->) (,) (Meet Word64)

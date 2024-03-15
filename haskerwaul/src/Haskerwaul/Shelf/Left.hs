{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Shelf.Left
  ( module Haskerwaul.Shelf.Left,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Lattice.Components
import Haskerwaul.Magma
import Numeric.Natural (Natural)
import Prelude (Integer)

-- |
-- = laws
--
--   [left self-distributive]: @x `op` (y `op` z) == (x `op` y) `op` (x `op` z)@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/shelf)
class (Magma c t a) => LeftShelf c t a

instance LeftShelf (->) (,) (Join Bool)

instance LeftShelf (->) (,) (Meet Bool)

instance LeftShelf (->) (,) (Join Int)

instance LeftShelf (->) (,) (Meet Int)

instance LeftShelf (->) (,) (Join Int8)

instance LeftShelf (->) (,) (Meet Int8)

instance LeftShelf (->) (,) (Join Int16)

instance LeftShelf (->) (,) (Meet Int16)

instance LeftShelf (->) (,) (Join Int32)

instance LeftShelf (->) (,) (Meet Int32)

instance LeftShelf (->) (,) (Join Int64)

instance LeftShelf (->) (,) (Meet Int64)

instance LeftShelf (->) (,) (Join Integer)

instance LeftShelf (->) (,) (Meet Integer)

instance LeftShelf (->) (,) (Join Natural)

instance LeftShelf (->) (,) (Meet Natural)

instance LeftShelf (->) (,) ()

instance LeftShelf (->) (,) (Join ())

instance LeftShelf (->) (,) (Meet ())

instance LeftShelf (->) (,) (Join Word)

instance LeftShelf (->) (,) (Meet Word)

instance LeftShelf (->) (,) (Join Word8)

instance LeftShelf (->) (,) (Meet Word8)

instance LeftShelf (->) (,) (Join Word16)

instance LeftShelf (->) (,) (Meet Word16)

instance LeftShelf (->) (,) (Join Word32)

instance LeftShelf (->) (,) (Meet Word32)

instance LeftShelf (->) (,) (Join Word64)

instance LeftShelf (->) (,) (Meet Word64)

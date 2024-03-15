{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Shelf.Right
  ( module Haskerwaul.Shelf.Right,

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
--   [right self-distributive]: @(x `op` y) `op` z == (x `op` z) `op` (y `op` z)@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/shelf)
class (Magma c t a) => RightShelf c t a

instance RightShelf (->) (,) (Join Bool)

instance RightShelf (->) (,) (Meet Bool)

instance RightShelf (->) (,) (Join Int)

instance RightShelf (->) (,) (Meet Int)

instance RightShelf (->) (,) (Join Int8)

instance RightShelf (->) (,) (Meet Int8)

instance RightShelf (->) (,) (Join Int16)

instance RightShelf (->) (,) (Meet Int16)

instance RightShelf (->) (,) (Join Int32)

instance RightShelf (->) (,) (Meet Int32)

instance RightShelf (->) (,) (Join Int64)

instance RightShelf (->) (,) (Meet Int64)

instance RightShelf (->) (,) (Join Integer)

instance RightShelf (->) (,) (Meet Integer)

instance RightShelf (->) (,) (Join Natural)

instance RightShelf (->) (,) (Meet Natural)

instance RightShelf (->) (,) ()

instance RightShelf (->) (,) (Join ())

instance RightShelf (->) (,) (Meet ())

instance RightShelf (->) (,) (Join Word)

instance RightShelf (->) (,) (Meet Word)

instance RightShelf (->) (,) (Join Word8)

instance RightShelf (->) (,) (Meet Word8)

instance RightShelf (->) (,) (Join Word16)

instance RightShelf (->) (,) (Meet Word16)

instance RightShelf (->) (,) (Join Word32)

instance RightShelf (->) (,) (Meet Word32)

instance RightShelf (->) (,) (Join Word64)

instance RightShelf (->) (,) (Meet Word64)

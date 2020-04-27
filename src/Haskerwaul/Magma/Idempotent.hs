{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Idempotent
  ( module Haskerwaul.Magma.Idempotent
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Integer)

import Haskerwaul.Lattice.Components
import Haskerwaul.Magma

-- |
-- = laws
--   [`Haskerwaul.Law.Idempotency.idempotency`]: @`op` a a == a@
class Magma c t a => IdempotentMagma c t a

instance IdempotentMagma (->) (,) (Join Bool)
instance IdempotentMagma (->) (,) (Meet Bool)

instance IdempotentMagma (->) (,) (Join Int)
instance IdempotentMagma (->) (,) (Meet Int)

instance IdempotentMagma (->) (,) (Join Int8)
instance IdempotentMagma (->) (,) (Meet Int8)

instance IdempotentMagma (->) (,) (Join Int16)
instance IdempotentMagma (->) (,) (Meet Int16)

instance IdempotentMagma (->) (,) (Join Int32)
instance IdempotentMagma (->) (,) (Meet Int32)

instance IdempotentMagma (->) (,) (Join Int64)
instance IdempotentMagma (->) (,) (Meet Int64)

instance IdempotentMagma (->) (,) (Join Integer)
instance IdempotentMagma (->) (,) (Meet Integer)

instance IdempotentMagma (->) (,) (Join Natural)
instance IdempotentMagma (->) (,) (Meet Natural)

instance IdempotentMagma (->) (,)       ()
instance IdempotentMagma (->) (,) (Join ())
instance IdempotentMagma (->) (,) (Meet ())

instance IdempotentMagma (->) (,) (Join Word)
instance IdempotentMagma (->) (,) (Meet Word)

instance IdempotentMagma (->) (,) (Join Word8)
instance IdempotentMagma (->) (,) (Meet Word8)

instance IdempotentMagma (->) (,) (Join Word16)
instance IdempotentMagma (->) (,) (Meet Word16)

instance IdempotentMagma (->) (,) (Join Word32)
instance IdempotentMagma (->) (,) (Meet Word32)

instance IdempotentMagma (->) (,) (Join Word64)
instance IdempotentMagma (->) (,) (Meet Word64)

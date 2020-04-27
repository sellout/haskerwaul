{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Commutative
  ( module Haskerwaul.Magma.Commutative
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Prelude (Integer)
import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)

import Haskerwaul.Lattice.Components
import Haskerwaul.Magma
import Haskerwaul.Semiring.Components

-- | [nLab](https://ncatlab.org/nlab/show/commutative+magma)
--
-- = laws
--   [`Haskerwaul.Law.Commutativity.commutativity`]: @`op` x y == `op` y x@
class Magma c t a => CommutativeMagma c t a

instance CommutativeMagma (->) (,) (Join Bool)
instance CommutativeMagma (->) (,) (Meet Bool)

instance CommutativeMagma (->) (,) (Additive       Int)
instance CommutativeMagma (->) (,) (Join           Int)
instance CommutativeMagma (->) (,) (Meet           Int)
instance CommutativeMagma (->) (,) (Multiplicative Int)

instance CommutativeMagma (->) (,) (Additive       Int8)
instance CommutativeMagma (->) (,) (Join           Int8)
instance CommutativeMagma (->) (,) (Meet           Int8)
instance CommutativeMagma (->) (,) (Multiplicative Int8)

instance CommutativeMagma (->) (,) (Additive       Int16)
instance CommutativeMagma (->) (,) (Join           Int16)
instance CommutativeMagma (->) (,) (Meet           Int16)
instance CommutativeMagma (->) (,) (Multiplicative Int16)

instance CommutativeMagma (->) (,) (Additive       Int32)
instance CommutativeMagma (->) (,) (Join           Int32)
instance CommutativeMagma (->) (,) (Meet           Int32)
instance CommutativeMagma (->) (,) (Multiplicative Int32)

instance CommutativeMagma (->) (,) (Additive       Int64)
instance CommutativeMagma (->) (,) (Join           Int64)
instance CommutativeMagma (->) (,) (Meet           Int64)
instance CommutativeMagma (->) (,) (Multiplicative Int64)

instance CommutativeMagma (->) (,) (Additive       Integer)
instance CommutativeMagma (->) (,) (Join           Integer)
instance CommutativeMagma (->) (,) (Meet           Integer)
instance CommutativeMagma (->) (,) (Multiplicative Integer)

instance CommutativeMagma (->) (,) (Additive             Natural)
instance CommutativeMagma (->) (,) (Join                 Natural)
instance CommutativeMagma (->) (,) (Meet                 Natural)
instance CommutativeMagma (->) (,) (Multiplicative       Natural)
instance CommutativeMagma (->) (,) (Additive       (Join Natural))
instance CommutativeMagma (->) (,) (Multiplicative (Join Natural))

instance CommutativeMagma (->) (,)                 ()
instance CommutativeMagma (->) (,) (Additive       ())
instance CommutativeMagma (->) (,) (Join           ())
instance CommutativeMagma (->) (,) (Meet           ())
instance CommutativeMagma (->) (,) (Multiplicative ())

instance CommutativeMagma (->) (,) (Additive       Word)
instance CommutativeMagma (->) (,) (Join           Word)
instance CommutativeMagma (->) (,) (Meet           Word)
instance CommutativeMagma (->) (,) (Multiplicative Word)

instance CommutativeMagma (->) (,) (Additive       Word8)
instance CommutativeMagma (->) (,) (Join           Word8)
instance CommutativeMagma (->) (,) (Meet           Word8)
instance CommutativeMagma (->) (,) (Multiplicative Word8)

instance CommutativeMagma (->) (,) (Additive       Word16)
instance CommutativeMagma (->) (,) (Join           Word16)
instance CommutativeMagma (->) (,) (Meet           Word16)
instance CommutativeMagma (->) (,) (Multiplicative Word16)

instance CommutativeMagma (->) (,) (Additive       Word32)
instance CommutativeMagma (->) (,) (Join           Word32)
instance CommutativeMagma (->) (,) (Meet           Word32)
instance CommutativeMagma (->) (,) (Multiplicative Word32)

instance CommutativeMagma (->) (,) (Additive       Word64)
instance CommutativeMagma (->) (,) (Join           Word64)
instance CommutativeMagma (->) (,) (Meet           Word64)
instance CommutativeMagma (->) (,) (Multiplicative Word64)

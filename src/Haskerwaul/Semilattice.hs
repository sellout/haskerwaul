{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semilattice
  ( module Haskerwaul.Semilattice
  -- * extended modules
  , module Haskerwaul.Semigroup.Commutative
  ) where

import           Prelude (Integer)
import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)

import Haskerwaul.Lattice.Components
import Haskerwaul.Semigroup.Commutative

-- | https://ncatlab.org/nlab/show/semilattice
class CommutativeSemigroup k t a => Semilattice k t a

instance Semilattice (->) (,) (Join Bool)
instance Semilattice (->) (,) (Meet Bool)

instance Semilattice (->) (,) (Join Int)
instance Semilattice (->) (,) (Meet Int)

instance Semilattice (->) (,) (Join Int8)
instance Semilattice (->) (,) (Meet Int8)

instance Semilattice (->) (,) (Join Int16)
instance Semilattice (->) (,) (Meet Int16)

instance Semilattice (->) (,) (Join Int32)
instance Semilattice (->) (,) (Meet Int32)

instance Semilattice (->) (,) (Join Int64)
instance Semilattice (->) (,) (Meet Int64)

instance Semilattice (->) (,) (Join Integer)
instance Semilattice (->) (,) (Meet Integer)

instance Semilattice (->) (,) (Join Natural)
instance Semilattice (->) (,) (Meet Natural)

instance Semilattice (->) (,)       ()
instance Semilattice (->) (,) (Join ())
instance Semilattice (->) (,) (Meet ())

instance Semilattice (->) (,) (Join Word)
instance Semilattice (->) (,) (Meet Word)

instance Semilattice (->) (,) (Join Word8)
instance Semilattice (->) (,) (Meet Word8)

instance Semilattice (->) (,) (Join Word16)
instance Semilattice (->) (,) (Meet Word16)

instance Semilattice (->) (,) (Join Word32)
instance Semilattice (->) (,) (Meet Word32)

instance Semilattice (->) (,) (Join Word64)
instance Semilattice (->) (,) (Meet Word64)

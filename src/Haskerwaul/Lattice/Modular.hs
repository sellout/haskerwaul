{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Modular
  ( module Haskerwaul.Lattice.Modular
  -- * extended modules
  , module Haskerwaul.Lattice
  ) where

import qualified Data.Bool as Base
import qualified Data.Int as Base
import qualified Data.Word as Base

import Haskerwaul.Lattice

-- | [nLab](https://ncatlab.org/nlab/show/modular+lattice)
class Lattice c t a => ModularLattice c t a

instance ModularLattice (->) (,) Base.Bool

instance ModularLattice (->) (,) Base.Int

instance ModularLattice (->) (,) Base.Int8

instance ModularLattice (->) (,) Base.Int16

instance ModularLattice (->) (,) Base.Int32

instance ModularLattice (->) (,) Base.Int64

instance ModularLattice (->) (,) Base.Word

instance ModularLattice (->) (,) Base.Word8

instance ModularLattice (->) (,) Base.Word16

instance ModularLattice (->) (,) Base.Word32

instance ModularLattice (->) (,) Base.Word64

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Distributive
  ( module Haskerwaul.Lattice.Distributive
  -- * extended modules
  , module Haskerwaul.Lattice.Modular
  ) where

import qualified Data.Bool as Base
import qualified Data.Int as Base
import qualified Data.Word as Base

import Haskerwaul.Lattice.Modular

-- | [nLab](https://ncatlab.org/nlab/show/distributive+lattice)
class ModularLattice c t a => DistributiveLattice c t a

instance DistributiveLattice (->) (,) Base.Bool

instance DistributiveLattice (->) (,) Base.Int

instance DistributiveLattice (->) (,) Base.Int8

instance DistributiveLattice (->) (,) Base.Int16

instance DistributiveLattice (->) (,) Base.Int32

instance DistributiveLattice (->) (,) Base.Int64

instance DistributiveLattice (->) (,) Base.Word

instance DistributiveLattice (->) (,) Base.Word8

instance DistributiveLattice (->) (,) Base.Word16

instance DistributiveLattice (->) (,) Base.Word32

instance DistributiveLattice (->) (,) Base.Word64

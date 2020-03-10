{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Distributive
  ( module Haskerwaul.Lattice.Distributive
  -- * extended modules
  , module Haskerwaul.Lattice
  ) where

import qualified Data.Bool as Base

import Haskerwaul.Lattice

-- | [nLab](https://ncatlab.org/nlab/show/distributive+lattice)
class Lattice c t a => DistributiveLattice c t a

instance DistributiveLattice (->) (,) Base.Bool

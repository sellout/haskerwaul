{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Complemented
  ( module Haskerwaul.Lattice.Complemented
  -- * extended modules
  , module Haskerwaul.Lattice.Bounded
  ) where

import qualified Data.Bool as Base

import Haskerwaul.Lattice.Bounded

-- | [nLab](https://ncatlab.org/nlab/show/complemented+lattice)
class BoundedLattice c t a => ComplementedLattice c t a

instance ComplementedLattice (->) (,) Base.Bool where

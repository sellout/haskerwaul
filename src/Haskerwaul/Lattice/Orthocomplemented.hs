{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Orthocomplemented
  ( module Haskerwaul.Lattice.Orthocomplemented
  -- * extended modules
  , module Haskerwaul.Lattice.Complemented.Uniquely
  ) where

import qualified Data.Bool as Base

import Haskerwaul.Lattice.Complemented.Uniquely

-- | [nLab](https://ncatlab.org/nlab/show/complemented+lattice)
class UniquelyComplementedLattice c t a => OrthocomplementedLattice c t a

instance OrthocomplementedLattice (->) (,) Base.Bool

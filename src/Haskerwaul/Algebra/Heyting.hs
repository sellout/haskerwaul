{-# language UndecidableSuperClasses #-}

module Haskerwaul.Algebra.Heyting
  ( module Haskerwaul.Algebra.Heyting
  -- * extended modules
  , module Haskerwaul.Lattice.Distributive.Bounded
  ) where

import qualified Data.Bool as Base

import Haskerwaul.Lattice.Distributive.Bounded

-- | [nLab](https://ncatlab.org/nlab/show/Heyting+algebra)
class BoundedDistributiveLattice c t a => HeytingAlgebra c t a

instance HeytingAlgebra (->) (,) Base.Bool

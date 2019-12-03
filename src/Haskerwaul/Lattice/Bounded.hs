{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Bounded
  ( module Haskerwaul.Lattice.Bounded
  -- * extended modules
  , module Haskerwaul.Lattice
  , module Haskerwaul.Semilattice.Bounded
  ) where

import Haskerwaul.Lattice
import Haskerwaul.Semilattice.Bounded

-- | https://ncatlab.org/nlab/show/lattice#bounded_lattices_and_pseudolattices
class ( Lattice c t a
      , BoundedSemilattice c t (Meet a)
      , BoundedSemilattice c t (Join a)) =>
      BoundedLattice c t a

instance ( Lattice c t a
         , BoundedSemilattice c t (Meet a)
         , BoundedSemilattice c t (Join a)) =>
         BoundedLattice c t a

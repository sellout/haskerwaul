{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Distributive.Bounded
  ( module Haskerwaul.Lattice.Distributive.Bounded
  -- * extended modules
  , module Haskerwaul.Lattice.Bounded
  , module Haskerwaul.Lattice.Distributive
  ) where

import Haskerwaul.Lattice.Bounded
import Haskerwaul.Lattice.Distributive

class (BoundedLattice c t a, DistributiveLattice c t a) =>
      BoundedDistributiveLattice c t a

instance (BoundedLattice c t a, DistributiveLattice c t a) =>
         BoundedDistributiveLattice c t a

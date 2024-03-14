module Haskerwaul.Lattice.Bounded.Laws where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Semilattice.Bounded.Laws
import Haskerwaul.Lattice.Bounded
import Haskerwaul.Lattice.Laws (LatticeLaws, latticeLaws)

data BoundedLatticeLaws c a =
  BoundedLatticeLaws
    { lattice :: LatticeLaws c a
    , meetSemilattice :: BoundedSemilatticeLaws c (Meet a)
    , joinSemilattice :: BoundedSemilatticeLaws c (Join a)
    }

boundedLatticeLaws ::
  (CartesianMonoidalCategory c, BoundedLattice c (Prod c) a) =>
  BoundedLatticeLaws c a
boundedLatticeLaws =
  BoundedLatticeLaws
    { lattice = latticeLaws
    , meetSemilattice = boundedSemilatticeLaws
    , joinSemilattice = boundedSemilatticeLaws
    }

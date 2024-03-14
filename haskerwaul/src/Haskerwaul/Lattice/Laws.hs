module Haskerwaul.Lattice.Laws where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Lattice
import Haskerwaul.Semilattice.Laws

data LatticeLaws c a = LatticeLaws
  { meetSemilattice :: SemilatticeLaws c (Meet a),
    joinSemilattice :: SemilatticeLaws c (Join a)
  }

latticeLaws ::
  (CartesianMonoidalCategory c, Lattice c (Prod c) a) => LatticeLaws c a
latticeLaws =
  LatticeLaws
    { meetSemilattice = semilatticeLaws,
      joinSemilattice = semilatticeLaws
    }

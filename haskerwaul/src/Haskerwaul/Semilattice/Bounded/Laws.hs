module Haskerwaul.Semilattice.Bounded.Laws where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Monoid.Commutative.Laws
import Haskerwaul.Monoid.Graphic.Laws
import Haskerwaul.Semilattice.Bounded
import Haskerwaul.Semilattice.Laws

data BoundedSemilatticeLaws c a = BoundedSemilatticeLaws
  { commutativeMonoid :: CommutativeMonoidLaws c (Prod c) a,
    graphicMonoid :: GraphicMonoidLaws c a,
    semilattice :: SemilatticeLaws c a
  }

boundedSemilatticeLaws ::
  (CartesianMonoidalCategory c, BoundedSemilattice c (Prod c) a) =>
  BoundedSemilatticeLaws c a
boundedSemilatticeLaws =
  BoundedSemilatticeLaws
    { commutativeMonoid = commutativeMonoidLaws,
      graphicMonoid = graphicMonoidLaws,
      semilattice = semilatticeLaws
    }

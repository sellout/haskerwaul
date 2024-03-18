{-# LANGUAGE Safe #-}

module Haskerwaul.Semilattice.Laws where

import Haskerwaul.Band.Laws
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Semigroup.Commutative.Laws
import Haskerwaul.Semilattice

data SemilatticeLaws c a = SemilatticeLaws
  { commutativeSemigroup :: CommutativeSemigroupLaws c (Prod c) a,
    band :: BandLaws c a
  }

semilatticeLaws :: (CartesianMonoidalCategory c, Semilattice c (Prod c) a) => SemilatticeLaws c a
semilatticeLaws =
  SemilatticeLaws
    { commutativeSemigroup = commutativeSemigroupLaws,
      band = bandLaws
    }

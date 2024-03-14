{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Algebra.Boolean
  ( module Haskerwaul.Algebra.Boolean
  -- * extended modules
  , module Haskerwaul.Algebra.Heyting
  , module Haskerwaul.Lattice.Orthocomplemented
  ) where

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Lattice.Orthocomplemented

-- | [nLab](https://ncatlab.org/nlab/show/Boolean+algebra)
--
--  __NB__: Instances for this are automatically coalesced.
class (OrthocomplementedLattice c t a, HeytingAlgebra c t a) =>
      BooleanAlgebra c t a

-- | Anything that is both an `OrthocomplementedLattice` and a `HeytingAlgebra`
--   is a `BooleanAlgebra`.
instance (OrthocomplementedLattice c t a, HeytingAlgebra c t a) =>
         BooleanAlgebra c t a

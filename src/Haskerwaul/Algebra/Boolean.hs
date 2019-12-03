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

-- | https://ncatlab.org/nlab/show/Boolean+algebra
class (OrthocomplementedLattice c t a, HeytingAlgebra c t a) =>
      BooleanAlgebra c t a

instance (OrthocomplementedLattice c t a, HeytingAlgebra c t a) =>
         BooleanAlgebra c t a

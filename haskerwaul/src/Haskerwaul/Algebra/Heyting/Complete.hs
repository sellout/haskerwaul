{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Algebra.Heyting.Complete
  ( module Haskerwaul.Algebra.Heyting.Complete
  -- * extended modules
  , module Haskerwaul.Algebra.Heyting
  , module Haskerwaul.Lattice.Complete
  ) where

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Lattice.Complete

-- | [nLab](https://ncatlab.org/nlab/show/complete+Heyting+algebra)
--
--  __NB__: Instances for this are automatically coalesced.
class (CompleteLattice c t a, HeytingAlgebra c t a) =>
      CompleteHeytingAlgebra c t a

instance (CompleteLattice c t a, HeytingAlgebra c t a) =>
         CompleteHeytingAlgebra c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Complete
  ( module Haskerwaul.Lattice.Complete
  -- * extended modules
  , module Haskerwaul.Lattice
  ) where

import Haskerwaul.Lattice

-- | https://ncatlab.org/nlab/show/complete+lattice
class Lattice c t a => CompleteLattice c t a

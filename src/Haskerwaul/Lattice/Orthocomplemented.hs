{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Orthocomplemented
  ( module Haskerwaul.Lattice.Orthocomplemented
  -- * extended modules
  , module Haskerwaul.Lattice.Complemented
  ) where

import qualified Data.Bool as Base
import           Data.Proxy (Proxy(..))

import Haskerwaul.Lattice.Complemented
import Haskerwaul.Object

-- | https://ncatlab.org/nlab/show/complemented+lattice
class ComplementedLattice c t a => OrthocomplementedLattice c t a where
  complement :: Ob c a => Proxy t -> a `c` a

instance OrthocomplementedLattice (->) (,) Base.Bool where
  complement Proxy = Base.not

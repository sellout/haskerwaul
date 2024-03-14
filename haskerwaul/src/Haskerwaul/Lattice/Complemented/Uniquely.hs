{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Complemented.Uniquely
  ( module Haskerwaul.Lattice.Complemented.Uniquely
  -- * extended modules
  , module Haskerwaul.Lattice.Complemented
  ) where

import qualified Data.Bool as Base
import           Data.Proxy (Proxy(..))

import Haskerwaul.Lattice.Complemented
import Haskerwaul.Object

-- | [Wikipedia](https://en.wikipedia.org/wiki/Complemented_lattice#Definition_and_basic_properties)
class ComplementedLattice c t a => UniquelyComplementedLattice c t a where
  complement :: Ob c a => Proxy t -> a `c` a

instance UniquelyComplementedLattice (->) (,) Base.Bool where
  complement Proxy = Base.not

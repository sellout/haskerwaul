{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice
  ( module Haskerwaul.Lattice
  -- * extended modules
  , module Haskerwaul.Lattice.Components
  , module Haskerwaul.Semilattice
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Lattice.Components
import Haskerwaul.Semilattice

-- | [nLab](https://ncatlab.org/nlab/show/lattice)
--
--  __NB__: Instances for this are automatically coalesced.
class (Semilattice c t (Meet a), Semilattice c t (Join a)) => Lattice c t a

meet :: (c ~ (->), SemigroupalCategory c t, Lattice c t a) => t a a `c` a
meet = getMeet . op . bimap Meet Meet

join :: (c ~ (->), SemigroupalCategory c t, Lattice c t a) => t a a `c` a
join = getJoin . op . bimap Join Join

instance (Semilattice c t (Meet a), Semilattice c t (Join a)) => Lattice c t a

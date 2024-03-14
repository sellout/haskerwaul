{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Bounded
  ( module Haskerwaul.Lattice.Bounded
  -- * extended modules
  , module Haskerwaul.Lattice
  , module Haskerwaul.Semilattice.Bounded
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Category.Monoidal
import Haskerwaul.Lattice
import Haskerwaul.Semilattice.Bounded

-- | [nLab](https://ncatlab.org/nlab/show/lattice#bounded_lattices_and_pseudolattices)
--
--  __NB__: Instances for this are automatically coalesced.
class ( Lattice c t a
      , BoundedSemilattice c t (Meet a)
      , BoundedSemilattice c t (Join a)) =>
      BoundedLattice c t a

top :: (c ~ (->), MonoidalCategory c t, BoundedLattice c t a)
    => Proxy t -> Unit c t `c` a
top t = getMeet . unit t

bottom :: (c ~ (->), MonoidalCategory c t, BoundedLattice c t a)
       => Proxy t -> Unit c t `c` a
bottom t = getJoin . unit t

instance ( Lattice c t a
         , BoundedSemilattice c t (Meet a)
         , BoundedSemilattice c t (Join a)) =>
         BoundedLattice c t a

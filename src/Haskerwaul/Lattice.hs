{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice
  ( module Haskerwaul.Lattice
  -- * extended modules
  , module Haskerwaul.Lattice.Components
  , module Haskerwaul.Semilattice
  ) where


import Haskerwaul.Bifunctor
import Haskerwaul.Lattice.Components
import Haskerwaul.Semigroupoid
import Haskerwaul.Semilattice

-- | [nLab](https://ncatlab.org/nlab/show/lattice)
class (Semilattice c t (Meet a), Semilattice c t (Join a)) => Lattice c t a

instance (Semilattice c t (Meet a), Semilattice c t (Join a)) => Lattice c t a

meet :: (c ~ (->), Lattice c t a, Bifunctor c c c t) => t a a `c` a
meet = getMeet . op . bimap Meet Meet

join :: (c ~ (->), Lattice c t a, Bifunctor c c c t) => t a a `c` a
join = getJoin . op . bimap Join Join

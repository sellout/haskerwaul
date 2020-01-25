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

-- | https://ncatlab.org/nlab/show/lattice
class (Semilattice k t (Meet a), Semilattice k t (Join a)) => Lattice k t a

instance (Semilattice k t (Meet a), Semilattice k t (Join a)) => Lattice k t a

meet :: (k ~ (->), Lattice k t a, Bifunctor k k k t) => t a a `k` a
meet = getMeet . op . bimap Meet Meet

join :: (k ~ (->), Lattice k t a, Bifunctor k k k t) => t a a `k` a
join = getJoin . op . bimap Join Join

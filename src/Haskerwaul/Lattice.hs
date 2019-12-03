{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Lattice
  ( module Haskerwaul.Lattice
  -- * extended modules
  , module Haskerwaul.Semilattice
  ) where

import           Prelude ((&&), (||), Bool(..))
import qualified Data.Monoid as Base
import qualified Data.Semigroup as Base

import Haskerwaul.Bifunctor
import Haskerwaul.Semigroupoid
import Haskerwaul.Semilattice

newtype Meet a = Meet { getMeet :: a }
newtype Join a = Join { getJoin :: a }

-- | https://ncatlab.org/nlab/show/lattice
class (Semilattice k t (Meet a), Semilattice k t (Join a)) => Lattice k t a

instance (Semilattice k t (Meet a), Semilattice k t (Join a)) => Lattice k t a

meet :: (k ~ (->), Lattice k t a, Bifunctor k k k t) => t a a `k` a
meet = getMeet . op . bimap Meet Meet

join :: (k ~ (->), Lattice k t a, Bifunctor k k k t) => t a a `k` a
join = getJoin . op . bimap Join Join

instance CommutativeMagma (->) (,) (Meet Bool)

instance Base.Semigroup (Meet Bool) where
  Meet x <> Meet y = Meet (x && y)

instance Base.Monoid (Meet Bool) where
  mempty = Meet True

instance Semilattice (->) (,) (Meet Bool)

instance CommutativeMagma (->) (,) (Join Bool)

instance Base.Semigroup (Join Bool) where
  Join x <> Join y = Join (x || y)

instance Base.Monoid (Join Bool) where
  mempty = Join False

instance Semilattice (->) (,) (Join Bool)

instance CommutativeMagma (->) (,) (Meet ())

instance Base.Semigroup (Meet ()) where
  Meet x <> Meet y = Meet (x Base.<> y)

instance Base.Monoid (Meet ()) where
  mempty = Meet ()

instance Semilattice (->) (,) (Meet ())

instance CommutativeMagma (->) (,) (Join ())

instance Base.Semigroup (Join ()) where
  Join x <> Join y = Join (x Base.<> y)

instance Base.Monoid (Join ()) where
  mempty = Join ()

instance Semilattice (->) (,) (Join ())

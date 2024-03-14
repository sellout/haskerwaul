{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Object.Dualizable
  ( module Haskerwaul.Object.Dualizable
  -- * extended modules
  , module Haskerwaul.Object.Dualizable.Left
  , module Haskerwaul.Object.Dualizable.Right
  ) where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy (..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Dualizable.Left
import Haskerwaul.Object.Dualizable.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/dualizable+object)
class (LeftDualizable c t a, RightDualizable c t a) => Dualizable c t a

instance (LeftDualizable c t a, RightDualizable c t a) => Dualizable c t a

-- instance (BraidedMonoidalCategory c t, Ob c a, Ob c (Dual c t a), Dualizable c t a) => RightDualizable c t a where
--   type RightDual c t a = Dual c t a
--   rightUnit = braid . leftCounit
--   rightCounit = leftUnit . braid

phi
  :: forall c t x y. (MonoidalCategory c t, LeftDualizable c t x, Ob c y)
  => Unit c t `c` t (LeftDual c t x) y -> x `c` y
phi f =
  to leftIdentity . first @c @c Proxy leftUnit . to assoc . second @c @c Proxy f . from rightIdentity
  \\ inT @(Ob c) @t @x @(LeftDual c t x)
  \\ inT @(Ob c) @t @(LeftDual c t x) @y

psi
  :: forall c t x y. (MonoidalCategory c t, LeftDualizable c t x, Ob c y)
  => x `c` y -> Unit c t `c` t (LeftDual c t x) y
psi g = second @c @c Proxy g . leftCounit

-- | This is like `trace` in that any morphism @x -> y@ is isomorphic to one @((), x) -> ((), y)@
trace'
  :: forall c t x. (BraidedMonoidalCategory c t, LeftDualizable c t x)
  => Proxy t -> x `c` x -> Unit c t `c` Unit c t
trace' Proxy f = leftUnit . to (braid @c @t) . psi f

dimension
  :: forall c t x. (BraidedMonoidalCategory c t, LeftDualizable c t x)
  => Proxy t -> Proxy x -> Unit c t `c` Unit c t
dimension t Proxy = trace' @_ @_ @x t id

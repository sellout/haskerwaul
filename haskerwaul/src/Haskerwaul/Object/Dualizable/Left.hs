{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Object.Dualizable.Left where

import Data.Constraint ((\\))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/dualizable+object)
class (MonoidalCategory c t, Ob c a, Ob c (LeftDual c t a)) => LeftDualizable (c :: ok -> ok -> Type) t a where
  type LeftDual c t a :: ok
  leftUnit :: t a (LeftDual c t a) `c` Unit c t
  leftCounit :: Unit c t `c` t (LeftDual c t a) a

leftDualMorphism ::
  forall c t a b.
  (MonoidalCategory c t, LeftDualizable c t a, LeftDualizable c t b) =>
  Proxy t ->
  a `c` b ->
  LeftDual c t b `c` LeftDual c t a
leftDualMorphism Proxy f =
  to rightIdentity
    . second @c @c Proxy leftUnit
    . second @c @c @c Proxy (first @c @c @c Proxy f)
    . from assoc
    . first @c @c @_ @t Proxy leftCounit
    . from leftIdentity
    \\ inT @(Ob c) @t @(LeftDual c t a) @a
    \\ inT @(Ob c) @t @a @(LeftDual c t b)
    \\ inT @(Ob c) @t @b @(LeftDual c t b)

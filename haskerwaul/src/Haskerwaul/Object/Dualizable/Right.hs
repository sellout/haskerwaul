{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Object.Dualizable.Right where

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
class (MonoidalCategory c t, Ob c a, Ob c (RightDual c t a)) => RightDualizable (c :: ok -> ok -> Type) t a where
  type RightDual c t a :: ok
  rightUnit :: Unit c t `c` t a (RightDual c t a)
  rightCounit :: t (RightDual c t a) a `c` Unit c t

rightDualMorphism ::
  forall c t a b.
  (RightDualizable c t a, RightDualizable c t b) =>
  Proxy t ->
  a `c` b ->
  RightDual c t b `c` RightDual c t a
rightDualMorphism Proxy f =
  to leftIdentity
    . first @c @c Proxy rightCounit
    . first @c @c @c Proxy (second @c @c @c Proxy f)
    . to assoc
    . second @c @c @_ @t Proxy rightUnit
    . from rightIdentity
    \\ inT @(Ob c) @t @a @(RightDual c t a)
    \\ inT @(Ob c) @t @(RightDual c t b) @a
    \\ inT @(Ob c) @t @(RightDual c t b) @b

{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Rigid.Left
  ( module Haskerwaul.Category.Monoidal.Rigid.Left,

    -- * extended modules
    module Haskerwaul.Category.Monoidal,
  )
where

import Data.Kind (Type)
import Haskerwaul.Category.Monoidal
import Haskerwaul.Object
import Haskerwaul.Object.Dualizable.Left

-- |
-- * references
--
-- - [nLab](https://ncatlab.org/nlab/show/rigid+monoidal+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Rigid_category)
--
-- __NB__: Instances for this are automatically coalesced.
class
  (forall a. (Ob c a) => LeftDualizable c t a) =>
  LeftRigidMonoidalCategory (c :: ok -> ok -> Type) t

instance
  (forall a. (Ob c a) => LeftDualizable c t a) =>
  LeftRigidMonoidalCategory (c :: ok -> ok -> Type) t

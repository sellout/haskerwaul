{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Rigid
  ( module Haskerwaul.Category.Monoidal.Rigid,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Rigid.Left,
    module Haskerwaul.Category.Monoidal.Rigid.Right,
  )
where

import Data.Kind (Type)
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Rigid.Left
import Haskerwaul.Category.Monoidal.Rigid.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/rigid+monoidal+category) -- "Nor does
--   this definition assert that the right dual of an object is isomorphic to
--   its left dual: this need not be the case in general, [...]"
--
--  __NB__: Instances for this are automatically coelesced.
class
  (LeftRigidMonoidalCategory c t, RightRigidMonoidalCategory c t) =>
  RigidMonoidalCategory (c :: ok -> ok -> Type) t

instance
  (LeftRigidMonoidalCategory c t, RightRigidMonoidalCategory c t) =>
  RigidMonoidalCategory c t

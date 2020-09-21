{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Rigid
  ( module Haskerwaul.Category.Monoidal.Rigid
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Rigid.Left
  , module Haskerwaul.Category.Monoidal.Rigid.Right
  ) where

import           Data.Kind (Type)

import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Rigid.Left
import Haskerwaul.Category.Monoidal.Rigid.Right
import Haskerwaul.Isomorphism

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/rigid+monoidal+category)
class (LeftRigidMonoidalCategory c t, RightRigidMonoidalCategory c t, LeftDual c t ~ RightDual c t) =>
      RigidMonoidalCategory (c :: ok -> ok -> Type) t

instance (LeftRigidMonoidalCategory c t, RightRigidMonoidalCategory c t, LeftDual c t ~ RightDual c t) =>
         RigidMonoidalCategory c t

leftInverseIso :: RigidMonoidalCategory c t => Isomorphism c (t (LeftDual c t a) a) (Unit c t)
leftInverseIso = Iso leftEpsilon rightEta

rightInverseIso :: RigidMonoidalCategory c t => Isomorphism c (t a (LeftDual c t a)) (Unit c t)
rightInverseIso = Iso rightEpsilon leftEta

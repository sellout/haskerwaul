{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Rigid.Left
  ( module Haskerwaul.Category.Monoidal.Rigid.Left
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Kind (Type)

import Haskerwaul.Category.Monoidal

class MonoidalCategory c t => LeftRigidMonoidalCategory (c :: ok -> ok -> Type) t where
  type LeftDual c t :: ok -> ok
  leftEta :: Unit c t `c` t a (LeftDual c t a)
  leftEpsilon :: t (LeftDual c t a) a `c` Unit c t

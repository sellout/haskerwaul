{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Rigid.Right
  ( module Haskerwaul.Category.Monoidal.Rigid.Right
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Kind (Type)

import Haskerwaul.Category.Monoidal

class MonoidalCategory c t => RightRigidMonoidalCategory (c :: ok -> ok -> Type) t where
  type RightDual c t :: ok -> ok
  rightEta :: Unit c t `c` t (RightDual c t a) a
  rightEpsilon :: t a (RightDual c t a) `c` Unit c t

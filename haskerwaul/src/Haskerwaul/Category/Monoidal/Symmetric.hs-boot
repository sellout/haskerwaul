{-# LANGUAGE Safe #-}
module Haskerwaul.Category.Monoidal.Symmetric where

import           Data.Kind (Type)

class SymmetricMonoidalCategory (c :: ok -> ok -> Type) (t :: ok -> ok -> ok)

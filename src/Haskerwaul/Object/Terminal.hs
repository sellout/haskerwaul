{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Object.Terminal where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Kind (Type)

import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

class Ob c (TerminalObject c) => HasTerminalObject (c :: ok -> ok -> Type) where
  type TerminalObject c :: ok
  (!) :: Ob c x => x `c` TerminalObject c

instance HasTerminalObject (->) where
  type TerminalObject (->) = ()
  (!) _ = ()

instance HasTerminalObject (:-) where
  type TerminalObject (:-) = ()
  (!) = Sub Dict

instance HasTerminalObject (NaturalTransformation (:-)) where
  type TerminalObject (NaturalTransformation (:-)) = All
  (!) = NT (Sub Dict)

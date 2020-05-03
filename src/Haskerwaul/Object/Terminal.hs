{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Object.Terminal where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Functor.Const (Const (..))
import           Data.Kind (Type)

import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Natural

-- | There are multiple classes (see
--  `Haskerwaul.Category.Monoidal.Cartesian.CartesianMonoidalCategory` or
--  `Haskerwaul.Category.Pointed.PointedCategory`) that have terminal objects.
--   This just gives us a common class for them to extend.
class Ob c (TerminalObject c) => HasTerminalObject (c :: ok -> ok -> Type) where
  -- | [nLab](https://ncatlab.org/nlab/show/terminal+object)
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

instance (c ~ (->), HasTerminalObject c) =>
         HasTerminalObject (NaturalTransformation c) where
  type TerminalObject (NaturalTransformation c) = Const (TerminalObject c)
  -- TODO: Replace this with `NT (first (!))`, but it currently causes an import
  --       cycle.
  (!) = NT (Const . (!))

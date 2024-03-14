{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Topos.Elementary
  ( module Haskerwaul.Topos.Elementary,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Closed.Cartesian,
  )
where

import Data.Bool (Bool (..))
import Data.Kind (Type)
import Haskerwaul.Algebra.Heyting
import Haskerwaul.Category.Monoidal.Closed.Cartesian
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/topos)
--
--  __TODO__: There should be a @`HeytingAlgebra` c (`Prod` c) (`Class` c)@
--            constraint here, but that currently forces our object to kind
--           `Type`, so we omit it to allow for more flexible instances.
class
  (CartesianClosedMonoidalCategory c, Ob c (Class c)) =>
  ElementaryTopos (c :: ok -> ok -> Type)
  where
  -- | The classifying object in the category.
  --   [nLab](https://ncatlab.org/nlab/show/classifying+space)
  type Class c :: ok

  -- | The subobject classifier.
  --   [nLab](https://ncatlab.org/nlab/show/subobject+classifier)
  --
  --  __NB__: This is a `Haskerwaul.Relation.Nullary.NullaryRelation`, but we
  --          can't use the synonym here because it creates an import cycle.
  true :: TerminalObject c `c` Class c

instance ElementaryTopos (->) where
  type Class (->) = Bool
  true () = True

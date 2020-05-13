{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Topos.Elementary
  ( module Haskerwaul.Topos.Elementary
  -- * extended modules
  , module Haskerwaul.Category.Closed.Cartesian
  ) where

import           Data.Bool (Bool(..))
import           Data.Kind (Type)

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full

-- | [nLab](https://ncatlab.org/nlab/show/topos)
--
--  __TODO__: There should be a @`HeytingAlgebra` c (`Prod` c) (`Class` c)@
--            constraint here, but that currently forces our object to kind
--           `Type`, so we omit it to allow for more flexible instances.
class (CartesianClosedCategory c, Ob c (Class c)) =>
      ElementaryTopos (c :: ok -> ok -> Type) where
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

instance ( ElementaryTopos c
         , TOb ob (Prod c)
         , ob (TerminalObject c)
         , TOb ob (Exp c)
         , ob (Class c)
         , ob (Meet (Class c))
         , ob (Join (Class c))) =>
         ElementaryTopos (FullSubcategory ob c) where
  type Class (FullSubcategory ob c) = Class c
  true = FS true

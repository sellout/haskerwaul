{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Topos.Elementary
  ( module Haskerwaul.Topos.Elementary
  -- * extended modules
  , module Haskerwaul.Category.Closed.Cartesian
  ) where

import           Data.Bool (Bool(..))

import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Subcategory.Full

-- | [nLab](https://ncatlab.org/nlab/show/topos)
class (CartesianClosedCategory c, Ob c (Class c)) => ElementaryTopos c where
  -- | The classifying object in the category.
  --   [nLab](https://ncatlab.org/nlab/show/classifying+space)
  type Class c
  -- | The subobject classifier.
  --   [nLab](https://ncatlab.org/nlab/show/subobject+classifier)
  true :: TerminalObject c `c` Class c

instance ElementaryTopos (->) where
  type Class (->) = Bool
  true () = True

instance ( ElementaryTopos c
         , TOb ob (Prod c)
         , ob (TerminalObject c)
         , TOb ob (Exp c)
         , ob (Class c)) =>
         ElementaryTopos (FullSubcategory ob c) where
  type Class (FullSubcategory ob c) = Class c
  true = FS true

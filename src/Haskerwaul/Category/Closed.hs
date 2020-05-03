{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed
  ( module Haskerwaul.Category.Closed
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import           Data.Constraint ((:-), (:=>))
import           Data.Kind (Type)

import Haskerwaul.Category
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/closed+category)
--
--  __TODO__: This should have a
--           @`Haskerwaul.Bifunctor.Bifunctor` (`Opposite` c) c c (`Exp` c)@
--            constraint, but it's been troublesome making an instance for
--            `(:=>)`, so we skip the constraint here and add it on the
--            instances that make use of it.
class (Category c, TOb (Ob c) (Exp c)) => ClosedCategory c where
  -- | [nLab](https://ncatlab.org/nlab/show/exponential+object)
  type Exp c :: ok -> ok -> ok

instance ClosedCategory (->) where
  type Exp (->) = (->)

data ExpTransformation (c :: Type -> Type -> Type) f g a =
  ET { runET :: f a `c` g a }

instance ClosedCategory c =>
         ClosedCategory (NaturalTransformation (c :: Type -> Type -> Type)) where
  type Exp (NaturalTransformation c) = ExpTransformation (Exp c)

instance ClosedCategory (:-) where
  type Exp (:-) = (:=>)

instance ClosedCategory (NaturalTransformation (:-)) where
  type Exp (NaturalTransformation (:-)) = ConstraintTransformation (:-)

instance (ClosedCategory c, TOb (Ob c) (Opposite (Exp c))) =>
         ClosedCategory (Opposite c) where
  type Exp (Opposite c) = Opposite (Exp c)
  
instance (ClosedCategory c, TOb ob (Exp c)) =>
         ClosedCategory (FullSubcategory ob c) where
  type Exp (FullSubcategory ob c) = Exp c

instance (ClosedCategory c, TOb (Ob c) (Isomorphism c)) =>
         ClosedCategory (Isomorphism c) where
  type Exp (Isomorphism c) = Isomorphism c

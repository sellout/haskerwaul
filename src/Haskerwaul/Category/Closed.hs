{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed
  ( module Haskerwaul.Category.Closed
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import           Data.Constraint ((:-), (:=>))

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
  -- | By default, the exponential object is the same as the arrow of the
  --   category. However, in some cases it is valid to "unwrap" the arrow. E.g.,
  --   in a `FullSubcategory`, the exponential is the same as the exponential of
  --   the category it restricts.
  type Exp c :: ok -> ok -> ok

instance ClosedCategory (->) where
  type Exp (->) = (->)

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

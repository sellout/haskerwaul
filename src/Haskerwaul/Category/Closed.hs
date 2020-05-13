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
import Haskerwaul.Category.Kleisli
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Monad
import Haskerwaul.Object
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

instance ClosedCategory d =>
         ClosedCategory (NaturalTransformation (d :: Type -> Type -> Type)) where
  type Exp (NaturalTransformation d) = ExpTransformation (Exp d)

instance ClosedCategory (:-) where
  type Exp (:-) = (:=>)

instance ClosedCategory (NaturalTransformation (:-)) where
  type Exp (NaturalTransformation (:-)) = ConstraintTransformation (:-)

instance (ClosedCategory c, TOb (Ob c) (Opposite (Exp c))) =>
         ClosedCategory (Opposite c) where
  type Exp (Opposite c) = Opposite (Exp c)
  
instance (ClosedCategory c, TOb (Ob c) (Isomorphism c)) =>
         ClosedCategory (Isomorphism c) where
  type Exp (Isomorphism c) = Isomorphism c

-- This instance can't be provided generically, I don't think. It does exist for
-- specific underlying categories, though (see the instance below).
-- instance (ClosedCategory c, Monad c m) => ClosedCategory (Kleisli c m) where
--   type Exp (Kleisli c m) = Kleisli (Exp c) m

instance {-# overlappable #-}
         ( Ob c ~ All
         , ClosedCategory c, Monad c m
         , BOb (Ob c) (Ob c) (Ob c) (Kleisli (Exp c) m)) =>
         ClosedCategory (Kleisli (c :: Type -> Type -> Type) m) where
  type Exp (Kleisli c m) = Kleisli (Exp c) m

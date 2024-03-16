{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed
  ( module Haskerwaul.Category.Closed,

    -- * extended modules
    module Haskerwaul.Category,
  )
where

import Data.Constraint ((:-), (:=>))
import Data.Kind (Type)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
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
--           @`Haskerwaul.Bifunctor.Bifunctor` (`Opposite` c) c c (`InternalHom` c)@
--            constraint, but it's been troublesome making an instance for
--            `(:=>)`, so we skip the constraint here and add it on the
--            instances that make use of it.
class (Category c, TOb (Ob c) (InternalHom c)) => ClosedCategory (c :: ok -> ok -> Type) where
  -- |
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/internal+hom)
  type InternalHom c :: ok -> ok -> ok

instance ClosedCategory (->) where
  type InternalHom (->) = (->)

-- | This is a natural transformation as an exponential object in a category
--   whose arrows are `NaturalTransformation`.
data ExpTransformation (c :: Type -> Type -> Type) f g a = ET {runET :: f a `c` g a}

#if MIN_VERSION_GLASGOW_HASKELL(9, 4, 0, 0)
-- |
--
--  __TODO__: GHC >= 9.4 complains about conflicting family instances for the
--            more general instance, so we restrict it on newer compilers.
instance ClosedCategory (NaturalTransformation c (->)) where
  type InternalHom (NaturalTransformation c (->)) = ExpTransformation (->)
#else
instance
  (ClosedCategory d, BOb (FOb (Ob c) (Ob d)) (FOb (Ob c) (Ob d)) (FOb (Ob c) (Ob d)) (ExpTransformation (InternalHom d))) =>
  ClosedCategory (NaturalTransformation c (d :: Type -> Type -> Type))
  where
  type InternalHom (NaturalTransformation c d) = ExpTransformation (InternalHom d)
#endif

instance ClosedCategory (:-) where
  type InternalHom (:-) = (:=>)

instance ClosedCategory (NaturalTransformation c (:-)) where
  type InternalHom (NaturalTransformation c (:-)) = ConstraintTransformation (:-)

instance
  (ClosedCategory c, TOb (Ob c) (Opposite (InternalHom c))) =>
  ClosedCategory (Opposite c)
  where
  type InternalHom (Opposite c) = Opposite (InternalHom c)

instance
  (ClosedCategory c, TOb (Ob c) (Isomorphism c)) =>
  ClosedCategory (Isomorphism c)
  where
  type InternalHom (Isomorphism c) = Isomorphism c

-- This instance can't be provided generically, I don't think. It does exist for
-- specific underlying categories, though (see the instance below).
-- instance (ClosedCategory c, Monad c m) => ClosedCategory (Kleisli c m) where
--   type InternalHom (Kleisli c m) = Kleisli (InternalHom c) m

instance
  {-# OVERLAPPABLE #-}
  ( Ob c ~ All,
    ClosedCategory c,
    Monad c m,
    BOb (Ob c) (Ob c) (Ob c) (Kleisli (InternalHom c) m)
  ) =>
  ClosedCategory (Kleisli (c :: Type -> Type -> Type) m)
  where
  type InternalHom (Kleisli c m) = Kleisli (InternalHom c) m

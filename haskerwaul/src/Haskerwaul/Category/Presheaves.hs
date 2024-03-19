{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Category.Presheaves where

import Data.Bool (Bool (..))
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Small
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Presheaf
import Haskerwaul.Topos.Elementary
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category+of+presheaves)
newtype PSh c f g = PSh (NaturalTransformation (Opposite c) (->) f g)

type instance Ob (PSh c) = Presheaf c

instance
  (Magmoid c) =>
  Magma (DinaturalTransformation (->)) Procompose (PSh c)
  where
  op = DT (\(Procompose (PSh f) (PSh g)) -> PSh (f . g))

instance
  (FlexibleMagmoid c) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose (PSh c)

instance
  (Semicategory c) =>
  Semigroup (DinaturalTransformation (->)) Procompose (PSh c)

instance
  (Category c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (PSh c)
  where
  unit Proxy = DT (\Refl -> PSh (NT id))

instance
  ( SmallCategory c,
    BOb (Presheaf c) (Presheaf c) (Presheaf c) (ExpTransformation (->))
  ) =>
  ClosedCategory (PSh c)
  where
  type InternalHom (PSh c) = ExpTransformation (->)

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category+of+presheaves#GeneralProperties), Proposition 2.3
instance
  ( SmallCategory c,
    BOb (Presheaf c) (Presheaf c) (Presheaf c) (ExpTransformation (->))
  ) =>
  CartesianClosedCategory (PSh c)
  where
  const = PSh (NT (ET . const))
  flattenHom = PSh (NT (\(ET f) -> ET (\a -> runET (f a) a)))

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  SemigroupalCategory (PSh c) (FTensor (,))
  where
  assoc =
    Iso
      (PSh (NT (\(FTensor (x, FTensor (y, z))) -> FTensor (FTensor (x, y), z))))
      (PSh (NT (\(FTensor (FTensor (x, y), z)) -> FTensor (x, FTensor (y, z)))))

instance (SmallCategory c) => MonoidalCategory' (PSh c) (FTensor (,)) where
  type Unit (PSh c) (FTensor (,)) = Const ()

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  MonoidalCategory (PSh c) (FTensor (,))
  where
  leftIdentity =
    Iso
      (PSh (NT (\(FTensor (Const (), a)) -> a)))
      (PSh (NT (\a -> FTensor (Const (), a))))
  rightIdentity =
    Iso
      (PSh (NT (\(FTensor (a, Const ())) -> a)))
      (PSh (NT (\a -> FTensor (a, Const ()))))

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  BraidedMonoidalCategory (PSh c) (FTensor (,))
  where
  braid =
    Iso
      (PSh (NT (\(FTensor (a, b)) -> FTensor (b, a))))
      (PSh (NT (\(FTensor (b, a)) -> FTensor (a, b))))

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  BalancedMonoidalCategory (PSh c) (FTensor (,))

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  SymmetricMonoidalCategory (PSh c) (FTensor (,))

instance (SmallCategory c) => HasTerminalObject (PSh c) where
  type TerminalObject (PSh c) = Const ()
  (!) = PSh (NT (\_ -> Const ()))

instance
  (SmallCategory c, Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))) =>
  CartesianMonoidalCategory (PSh c)
  where
  type Prod (PSh c) = (FTensor (,))
  exl = PSh (NT (\(FTensor (a, _)) -> a))
  exr = PSh (NT (\(FTensor (_, b)) -> b))
  diagonal = PSh (NT (\a -> FTensor (a, a)))

instance
  ( SmallCategory c,
    BOb (Presheaf c) (Presheaf c) (Presheaf c) (ExpTransformation (->)),
    Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))
  ) =>
  ClosedMonoidalCategory (PSh c) (FTensor (,))
  where
  curry (PSh (NT f)) = PSh (NT (\x -> ET (\y -> f (FTensor (x, y)))))
  uncurry (PSh (NT f)) = PSh (NT (\(FTensor (x, y)) -> runET (f x) y))

instance
  ( SmallCategory c,
    BOb (Presheaf c) (Presheaf c) (Presheaf c) (ExpTransformation (->)),
    Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))
  ) =>
  CartesianClosedMonoidalCategory (PSh c)
  where
  tuple = PSh (NT (\a -> ET (\b -> FTensor (a, b))))

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/category+of+presheaves#GeneralProperties), Proposition 2.4
instance
  ( SmallCategory c,
    BOb (Presheaf c) (Presheaf c) (Presheaf c) (ExpTransformation (->)),
    Bifunctor (PSh c) (PSh c) (PSh c) (FTensor (,))
  ) =>
  ElementaryTopos (PSh c)
  where
  type Class (PSh c) = Const Bool
  true = PSh (NT (\(Const ()) -> Const True))

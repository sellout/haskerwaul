{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Category.Kleisli where

import Data.Constraint ((\\))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality ((:~:) (..), type (~))
#else
import Data.Type.Equality ((:~:) (..))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Constraint
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Monad
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural

-- | [nLab](https://ncatlab.org/nlab/show/Kleisli+category)
--
--  __TODO__: Implement this as
-- --> type Kleisli c m = FullSubcategory FreeAlgebra (EilenbergMoore c m)
newtype Kleisli (c :: ok -> ok -> Type) m a b = Kleisli {runKleisli :: a `c` m b}

type instance Ob (Kleisli c _) = Ob c

instance
  (Ob c ~ All, Magmoid c, Monad c m) =>
  Magma (DinaturalTransformation (->)) Procompose (Kleisli c m)
  where
  op =
    DT (\(Procompose (Kleisli f) (Kleisli g)) -> Kleisli (flatten . map f . g))

instance
  (Ob c ~ All, FlexibleMagmoid c, Monad c m) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose (Kleisli c m)

instance
  (Ob c ~ All, Semicategory c, Monad c m) =>
  Semigroup (DinaturalTransformation (->)) Procompose (Kleisli c m)

instance
  (Ob c ~ All, Category c, Monad c m) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (Kleisli c m)
  where
  unit Proxy = DT (\Refl -> Kleisli pure)

instance
  ( Ob c ~ All,
    SemigroupalCategory c t,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t,
    Monad c m
  ) =>
  SemigroupalCategory (Kleisli c m) t
  where
  assoc = Iso (Kleisli (pure . to assoc)) (Kleisli (pure . from assoc))

instance (MonoidalCategory' c t) => MonoidalCategory' (Kleisli c m) t where
  type Unit (Kleisli c m) t = Unit c t

instance
  ( Ob c ~ All,
    MonoidalCategory c t,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t,
    Monad c m
  ) =>
  MonoidalCategory (Kleisli c m) t
  where
  leftIdentity =
    Iso (Kleisli (pure . to leftIdentity)) (Kleisli (pure . from leftIdentity))
  rightIdentity =
    Iso
      (Kleisli (pure . to rightIdentity))
      (Kleisli (pure . from rightIdentity))

instance (Category c, Monad c m, Magma c t a) => Magma (Kleisli c m) t a where
  op = Kleisli (pure . op)

instance
  (Category c, Monad c m, FlexibleMagma c t a) =>
  FlexibleMagma (Kleisli c m) t a

instance (Category c, Monad c m, Semigroup c t a) => Semigroup (Kleisli c m) t a

instance
  (Category c, Monad c m, UnitalMagma c t a) =>
  UnitalMagma (Kleisli c m) t a
  where
  unit p = Kleisli (pure . unit p)

instance
  ( Ob c ~ All,
    BraidedMonoidalCategory c t,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t,
    Monad c m
  ) =>
  BraidedMonoidalCategory (Kleisli c m) t
  where
  braid = Iso (Kleisli (pure . to braid)) (Kleisli (pure . from braid))

instance
  ( Ob c ~ All,
    BalancedMonoidalCategory c t,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t,
    Monad c m
  ) =>
  BalancedMonoidalCategory (Kleisli c m) t
  where
  balance t = Kleisli (pure . balance t)

instance
  ( Ob c ~ All,
    SymmetricMonoidalCategory c t,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t,
    Monad c m
  ) =>
  SymmetricMonoidalCategory (Kleisli c m) t

instance
  (CartesianMonoidalCategory c, Monad c m) =>
  HasTerminalObject (Kleisli c m)
  where
  type TerminalObject (Kleisli c m) = TerminalObject c
  (!) = Kleisli (pure . (!))

instance
  ( Ob c ~ All,
    CartesianMonoidalCategory c,
    Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) (Prod c),
    Monad c m
  ) =>
  CartesianMonoidalCategory (Kleisli c m)
  where
  type Prod (Kleisli c m) = Prod c
  exl = Kleisli (pure . exl)
  exr = Kleisli (pure . exr)
  diagonal = Kleisli (pure . diagonal)

-- | A monad in a Kleisli category is basically a traversable monad.
instance
  (Semicategory c, Monad c n, Functor (Kleisli c n) (Kleisli c n) m, Monad c m) =>
  Monad (Kleisli c n) m
  where
  pure :: forall a. (Ob c a) => Kleisli c n a (m a)
  pure = Kleisli (pure . pure) \\ inF @(Ob c) @(Ob c) @m @a
  flatten :: forall a. (Ob c a) => Kleisli c n (m (m a)) (m a)
  flatten = Kleisli (pure . flatten) \\ inF @(Ob c) @(Ob c) @m @a

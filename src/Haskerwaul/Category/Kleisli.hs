{-# language UndecidableInstances #-}

module Haskerwaul.Category.Kleisli where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Bifunctor
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Monad
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/Kleisli+category)
--
--  __TODO__: Implement this as
-- >>> type Kleisli c m = FullSubcategory FreeAlgebra (EilenbergMoore c m)
newtype Kleisli (c :: ok -> ok -> Type) m a b =
  Kleisli { runKleisli :: a `c` m b }

type instance Ob (Kleisli c m) = Ob c

instance (c ~ (->), Semigroupoid c, Monad c m) =>
         Magma (NaturalTransformation2 (->)) CProd (Kleisli c m) where
  op = NT2 (\(CProd (Kleisli f) (Kleisli g)) ->
              Kleisli (runNT op . Compose . map f . g))

instance (c ~ (->), Semigroupoid c, Monad c m) =>
         Semigroup (NaturalTransformation2 (->)) CProd (Kleisli c m)

instance (c ~ (->), Category c, Monad c m) =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (Kleisli c m) where
  unit Proxy =
    NT2 (\Refl -> Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity))

instance ( c ~ (->)
         , SemigroupalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         SemigroupalCategory (Kleisli c m) t where
  assoc =
    Iso
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . to assoc))
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . from assoc))

instance (MonoidalCategory' c t) => MonoidalCategory' (Kleisli c m) t where
  type Unit (Kleisli c m) t = Unit c t

instance ( c ~ (->)
         , MonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         MonoidalCategory (Kleisli c m) t where
  leftIdentity =
    Iso
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . to leftIdentity))
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . from leftIdentity))
  rightIdentity =
    Iso
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . to rightIdentity))
    (Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . from rightIdentity))

instance (c ~ (->), ClosedCategory c, Monad c m) =>
         ClosedCategory (Kleisli c m) where
  type Exp (Kleisli c m) = Kleisli (Exp c) m

instance ( c ~ (->)
         , ClosedMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         ClosedMonoidalCategory (Kleisli c m) t where
  apply = Kleisli (apply . first (Proxy :: Proxy c) runKleisli)

instance ( c ~ (->)
         , BraidedMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         BraidedMonoidalCategory (Kleisli c m) t where
  braid = Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . braid)

instance ( c ~ (->)
         , SymmetricMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         SymmetricMonoidalCategory (Kleisli c m) t

instance (c ~ (->), CartesianMonoidalCategory c, Monad c m) =>
         HasTerminalObject (Kleisli c m) where
  type TerminalObject (Kleisli c m) = TerminalObject c
  (!) = Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity .  (!))

instance ( c ~ (->)
         , CartesianMonoidalCategory c
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) (Prod c)
         , Monad c m) =>
         CartesianMonoidalCategory (Kleisli c m) where
  type Prod (Kleisli c m) = Prod c
  exl = Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . exl)
  exr = Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity . exr)
  diagonal =
    Kleisli (runNT (unit (Proxy :: Proxy Compose)) . Identity .  diagonal)

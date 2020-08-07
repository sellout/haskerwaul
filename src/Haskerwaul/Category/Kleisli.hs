{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Category.Kleisli where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Bifunctor
import Haskerwaul.Constraint
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Monad
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural

-- | [nLab](https://ncatlab.org/nlab/show/Kleisli+category)
--
--  __TODO__: Implement this as
-- >>> type Kleisli c m = FullSubcategory FreeAlgebra (EilenbergMoore c m)
newtype Kleisli (c :: ok -> ok -> Type) m a b =
  Kleisli { runKleisli :: a `c` m b }

type instance Ob (Kleisli c m) = Ob c

instance (Ob c ~ All, Semigroupoid c, Monad c m, FOb (Ob c) (Ob c) m) =>
         Magma (DinaturalTransformation (->)) Procompose (Kleisli c m) where
  op =
    DT (\(Procompose (Kleisli f) (Kleisli g)) -> Kleisli (flatten . map f . g))

instance (Ob c ~ All, Semigroupoid c, Monad c m) =>
         Semigroup (DinaturalTransformation (->)) Procompose (Kleisli c m)

instance (Ob c ~ All, Category c, Monad c m) =>
         UnitalMagma (DinaturalTransformation (->)) Procompose (Kleisli c m) where
  unit Proxy = DT (\Refl -> Kleisli pure)

instance ( Ob c ~ All
         , SemigroupalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         SemigroupalCategory (Kleisli c m) t where
  assoc = Iso (Kleisli (pure . to assoc)) (Kleisli (pure . from assoc))

instance (MonoidalCategory' c t) => MonoidalCategory' (Kleisli c m) t where
  type Unit (Kleisli c m) t = Unit c t

instance ( Ob c ~ All
         , MonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         MonoidalCategory (Kleisli c m) t where
  leftIdentity =
    Iso (Kleisli (pure . to leftIdentity)) (Kleisli (pure . from leftIdentity))
  rightIdentity =
    Iso
    (Kleisli (pure . to rightIdentity))
    (Kleisli (pure . from rightIdentity))

instance ( Ob c ~ All
         , BraidedMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         BraidedMonoidalCategory (Kleisli c m) t where
  braid = Kleisli (pure . braid)

instance ( Ob c ~ All
         , SymmetricMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         SymmetricMonoidalCategory (Kleisli c m) t

instance (CartesianMonoidalCategory c, Monad c m) =>
         HasTerminalObject (Kleisli c m) where
  type TerminalObject (Kleisli c m) = TerminalObject c
  (!) = Kleisli (pure .  (!))

instance ( Ob c ~ All
         , CartesianMonoidalCategory c
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) (Prod c)
         , Monad c m) =>
         CartesianMonoidalCategory (Kleisli c m) where
  type Prod (Kleisli c m) = Prod c
  exl = Kleisli (pure . exl)
  exr = Kleisli (pure . exr)
  diagonal = Kleisli (pure . diagonal)

-- | A monad in a Kleisli category is basically a traversable monad.
instance (Semigroupoid c, Monad c n, Functor (Kleisli c n) (Kleisli c n) m, Monad c m) =>
         Monad (Kleisli c n) m where
  pure = Kleisli (pure . pure) 
  flatten = Kleisli (pure . flatten)

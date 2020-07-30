{-# language UndecidableInstances #-}

module Haskerwaul.Category.Opposite where

import           Control.Arrow ((&&&))
import           Data.Constraint ((:-)(..), Bottom(..), bottom)
import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Either (Either(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Tuple as Base
import qualified Data.Void as Base

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Constraint
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Monad
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/opposite+category)
newtype Opposite c a b = Opposite { opposite :: b `c` a }

type instance Ob (Opposite c) = Ob c

-- instance (FOb (Ob c) (Ob c) f, opOb ~ Ob (Opposite c)) => FOb opOb opO f where
--   inF = Sub Dict

-- instance BOb cOb dOb eOb c => BOb dOb cOb eOb (Opposite c) where
--   inB :: forall x y. (cOb x, dOb y, BOb cOb dOb eOb (c x y)) :- BOb dOb cOb eOb (Op c x y)
--   inB = Sub (Dict \\ inB @cOb @dOb @eOb @c @x @y)

-- | If /C/ is a `Semigroupoid`, then so is /C^op/.
instance Magma (NaturalTransformation2 (->)) Procompose c =>
         Magma (NaturalTransformation2 (->)) Procompose (Opposite c) where
  op = NT2 (\(Procompose (Opposite f) (Opposite g)) -> Opposite (g . f))

-- | If /C/ is a `Semigroupoid`, then so is /C^op/.
instance Semigroup (NaturalTransformation2 (->)) Procompose c =>
         Semigroup (NaturalTransformation2 (->)) Procompose (Opposite c)

-- | If /C/ is a `Category`, then so is /C^op/.
instance UnitalMagma (NaturalTransformation2 (->)) Procompose c =>
         UnitalMagma (NaturalTransformation2 (->)) Procompose (Opposite c) where
  unit Proxy = NT2 (\Refl -> Opposite id)

-- | If /C/ is a `MonoidalCategory`, then so is /C^op/.
instance MonoidalCategory' c t => MonoidalCategory' (Opposite c) t where
  type Unit (Opposite c) t = Unit c t

instance HasTerminalObject (Opposite (->)) where
  type TerminalObject (Opposite (->)) = Base.Void
  (!) = Opposite Base.absurd

instance HasTerminalObject (Opposite (:-)) where
  type TerminalObject (Opposite (:-)) = Bottom
  (!) = Opposite bottom

instance HasTerminalObject (Opposite (NaturalTransformation (:-))) where
  type TerminalObject (Opposite (NaturalTransformation (:-))) = None
  (!) = Opposite (NT none)

instance HasTerminalObject (Isomorphism c) =>
         HasTerminalObject (Opposite (Isomorphism c)) where
  type TerminalObject (Opposite (Isomorphism c)) =
    TerminalObject (Isomorphism c)
  (!) = Opposite (Iso (from (!)) (to (!)))

instance HasTerminalObject (Isomorphism c) =>
         HasTerminalObject (Isomorphism (Opposite c)) where
  type TerminalObject (Isomorphism (Opposite c)) =
    TerminalObject (Isomorphism c)
  (!) = Iso (Opposite (from (!))) (Opposite (to (!)))

-- | This type family avoids deep nesting of `Opposite` constructors by simply
--   unwrapping it if we''re taking the opposite of an opposite category (i.e.,
--  `Op` is its own inverse).
--
--  __NB__: This only works because currently all categories are __Set__-
--          enriched. If we add support for other /V/-categories, then we need
--          to ensure that /V/ is at least a
--         `Haskerwaul.Category.Monoidal.Symmetric.SymmetricMonoidalCategory` to
--          get a unique opposite category for it.
type family Op (c :: ok -> ok -> Type) :: ok -> ok -> Type where
  Op (Opposite c)                      = c
  Op (Isomorphism (Opposite c))        = Isomorphism c
  -- CompactClosedCategory c => Op c      = c
  Op c                                 = Opposite c

-- | The dual of an `Isomorphism` is an `Isomorphism` in the `Opposite`
--   category.
opIsomorphism
  :: Isomorphism
     (NaturalTransformation2 (->))
     (Opposite (Isomorphism c))
     (Isomorphism (Opposite c))
opIsomorphism =
  Iso
  (NT2 (Base.uncurry Iso . (Opposite . to &&& Opposite . from) . opposite))
  (NT2 (Opposite . Base.uncurry Iso . (opposite . to &&& opposite . from)))

opOpIso :: Isomorphism (NaturalTransformation2 (->)) (Opposite (Opposite c)) c
opOpIso = Iso (NT2 (opposite . opposite)) (NT2 (Opposite . Opposite))

-- | Natural transformation that converts an `Isomorphism` to an `Isomorphism`
--   in the opposite category.
isomorphismOp
  :: Isomorphism
     (NaturalTransformation2 (->))
     (Isomorphism c)
     (Isomorphism (Opposite c))
isomorphismOp =
  Iso
  (NT2 (Base.uncurry Iso . (Opposite . from &&& Opposite . to)))
  (NT2 (Base.uncurry Iso . (opposite . from &&& opposite . to)))

-- | Functors are self-dual.
instance {-# overlappable #-} Functor c d f =>
                              Functor (Opposite c) (Opposite d) f where
  map (Opposite f) = Opposite (map f)

instance (Semigroupoid c1, Bifunctor c1 c2 d t) =>
         Bifunctor (Opposite c1) (Opposite c2) (Opposite d) t where
  bimap f g = Opposite (bimap (opposite f) (opposite g))

-- | The arrow of every `Semigroupoid` is a `Haskerwaul.Profunctor.Profunctor`.
instance Semigroupoid c => Bifunctor (Opposite c) c (->) c where
  bimap f g fn = g . fn . opposite f

-- instance Bifunctor (Opposite (:-)) (:-) (:-) (:=>) where
--   -- bimap :: b :- a -> c :- d -> (a :=> c) :- (b :=> d)
--   bimap f g = trans g (trans ins (opposite f))

instance {-# OVERLAPPABLE #-} Monad' (Opposite (->)) m => Monad (Opposite (->)) m where
  pure = runNT (unit (Proxy :: Proxy Compose)) . Opposite runIdentity
  flatten = runNT op . Opposite getCompose

-- | Every semigroupal structure is semigroupal in the opposite category.
instance SemigroupalCategory c t => SemigroupalCategory (Opposite c) t where
  assoc = runNT2 (to isomorphismOp) assoc

-- | Every monoidal structure is monoidal in the opposite category.
instance MonoidalCategory c t => MonoidalCategory (Opposite c) t where
  leftIdentity = runNT2 (to isomorphismOp) leftIdentity
  rightIdentity = runNT2 (to isomorphismOp) rightIdentity

-- `CocartesianCategory` instances (in this module to avoid orphans)
instance CartesianMonoidalCategory (Opposite (->)) where
  type Prod (Opposite (->)) = Either
  exl = Opposite Left
  exr = Opposite Right
  diagonal =
    Opposite (\case
                 Left x -> x
                 Right x -> x)

instance BraidedMonoidalCategory c t =>
         BraidedMonoidalCategory (Opposite c) t where
  braid = Opposite braid

instance SymmetricMonoidalCategory c t =>
         SymmetricMonoidalCategory (Opposite c) t

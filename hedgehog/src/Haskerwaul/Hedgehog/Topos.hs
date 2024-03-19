{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

-- | This defines an `ElementaryTopos` instance for Hedgehog, such that it can
--   be used to apply all of the properties defined in Haskerwaul.
module Haskerwaul.Hedgehog.Topos where

import safe Control.Applicative
import safe Data.Eq (Eq)
import safe Data.Type.Equality ((:~:) (..))
import safe Haskerwaul.Bifunctor
import safe Haskerwaul.Constraint
import safe Haskerwaul.Isomorphism
import safe Haskerwaul.Object
import safe Haskerwaul.Object.Terminal
import safe Haskerwaul.Relation.Equality
import safe Haskerwaul.Topos.Elementary
import safe Haskerwaul.Transformation.Dinatural
import Hedgehog
import safe System.IO
import safe Text.Show (Show)

-- | A trivial newtype over __Hask__ that lets us choose a different classifying
--   object (i.e., `Property`-like instead of `Data.Bool.Bool`).
--
--  __NB__: This is likely better than something like @`runHH` :: `Gen` a ->
--         `PropertyT` `IO` b@ as it makes it easy to lift basically anything
--          into `HH`, and then we only need `Gen` and `PropertyT` at the
--          boundaries, which also lets us use custom display functions, etc.
newtype HH a b = HH {runHH :: a -> b}

type instance Ob HH = All

instance Magma (DinaturalTransformation (->)) Procompose HH where
  op = DT (\(Procompose (HH f) (HH g)) -> HH (runDT op (Procompose f g)))

instance FlexibleMagma (DinaturalTransformation (->)) Procompose HH

instance Semigroup (DinaturalTransformation (->)) Procompose HH

instance UnitalMagma (DinaturalTransformation (->)) Procompose HH where
  unit p = DT (\Refl -> HH (runDT (unit p) Refl))

instance (SemigroupalCategory (->) t) => SemigroupalCategory HH t where
  assoc = Iso (HH (to assoc)) (HH (from assoc))

instance (MonoidalCategory' (->) t) => MonoidalCategory' HH t where
  type Unit HH t = Unit (->) t

instance (MonoidalCategory (->) t) => MonoidalCategory HH t where
  leftIdentity = Iso (HH (to leftIdentity)) (HH (from leftIdentity))
  rightIdentity = Iso (HH (to rightIdentity)) (HH (from rightIdentity))

instance (BraidedMonoidalCategory (->) t) => BraidedMonoidalCategory HH t where
  braid = Iso (HH (to braid)) (HH (from braid))

instance (BalancedMonoidalCategory (->) t) => BalancedMonoidalCategory HH t where
  balance = HH . balance

instance (SymmetricMonoidalCategory (->) t) => SymmetricMonoidalCategory HH t

instance HasTerminalObject HH where
  type TerminalObject HH = ()
  (!) = HH (!)

instance CartesianMonoidalCategory HH where
  type Prod HH = (,)
  exl = HH exl
  exr = HH exr
  diagonal = HH diagonal

instance ClosedCategory HH where
  type InternalHom HH = (->)

instance (ClosedMonoidalCategory (->) t) => ClosedMonoidalCategory HH t where
  apply = HH apply
  curry (HH f) = HH (curry f)

instance CartesianClosedCategory HH where
  const = HH const
  flattenHom = HH flattenHom

instance CartesianClosedMonoidalCategory HH where
  tuple = HH tuple

instance ElementaryTopos HH where
  type Class HH = PropertyT IO ()
  true = HH pure

instance (Bifunctor (->) (->) (->) f) => Bifunctor HH HH HH f where
  bimap (HH f) (HH g) = HH (bimap f g)

instance (Magma (->) t a) => Magma HH t a where
  op = HH op

instance (FlexibleMagma (->) t a) => FlexibleMagma HH t a

instance (Semigroup (->) t a) => Semigroup HH t a

instance (UnitalMagma (->) t a) => UnitalMagma HH t a where
  unit p = HH (unit p)

-- | The relation hierarchy in Haskerwaul is odd. But, for now, all we care
--   about in the `HH` topos is equivalence, so we push everything through that.
instance (Eq a, Show a) => HomogeneousRelation' HH a where
  rel = HH (uncurry (===))

instance (Eq a, Show a) => Preorder HH a

instance (Eq a, Show a) => PartialOrder HH a

instance (Eq a, Show a) => PartialEquivalenceRelation HH a

instance (Eq a, Show a) => ToleranceRelation HH a

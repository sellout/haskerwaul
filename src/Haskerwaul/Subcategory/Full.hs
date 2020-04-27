{-# language UndecidableInstances #-}

module Haskerwaul.Subcategory.Full where

import           Control.Arrow ((&&&))
import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Constraint, Type)
import qualified Data.Ord as Base
import           Data.Proxy (Proxy(..))
import qualified Data.Set as Set
import qualified Data.Tuple as Base

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Monoidal
import Haskerwaul.Constraint
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Complemented.Uniquely
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/full+subcategory)
--
--  `Haskerwaul.Magma.Unital.unit` (on objects) and `inclusion` (on morphisms)
--   form an [inclusion functor](https://ncatlab.org/nlab/show/subcategory).
--
-- __NB__: This is probably less restrictive than you think. E.g.,
--        @`FullSubcategory` (`Monoid` (->) (,)) (->)@ is /not/
--        [Mon(Hask)](https://ncatlab.org/nlab/show/category+of+monoids). The
--         objects are the same, but the morphisms are simply Haskell functions,
--        /not/ monoid homomorphisms.
newtype FullSubcategory (ob :: ok -> Constraint) (c :: ok -> ok -> Type) a b =
  FS { inclusion :: a `c` b }

type instance Ob (FullSubcategory ob c) = CFProd (Ob c) ob

instance Magma (NaturalTransformation2 (->)) CProd c =>
         Magma (NaturalTransformation2 (->)) CProd (FullSubcategory ob c) where
  op = NT2 (\(CProd (FS f) (FS g)) -> FS (f . g))

instance Semigroup (NaturalTransformation2 (->)) CProd c =>
         Semigroup (NaturalTransformation2 (->)) CProd (FullSubcategory ob c)

instance (MonoidalCategory' c t, ob (Unit c t)) =>
         MonoidalCategory' (FullSubcategory ob c) t where
  type Unit (FullSubcategory ob c) t = Unit c t

instance UnitalMagma (NaturalTransformation2 (->)) CProd c =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (FullSubcategory ob c) where
  unit Proxy = NT2 (\Refl -> FS id)

instance LeftQuasigroup (NaturalTransformation2 (->)) CProd c =>
         LeftQuasigroup
         (NaturalTransformation2 (->))
         CProd
         (FullSubcategory ob c) where
  leftQuotient = NT2 FS . leftQuotient . bimap (NT2 inclusion) (NT2 inclusion)

instance RightQuasigroup (NaturalTransformation2 (->)) CProd c =>
         RightQuasigroup
         (NaturalTransformation2 (->))
         CProd
         (FullSubcategory ob c) where
  rightQuotient = NT2 FS . rightQuotient . bimap (NT2 inclusion) (NT2 inclusion)

instance {-# overlappable #-} (SemigroupalCategory c t, TOb ob t) =>
                              SemigroupalCategory (FullSubcategory ob c) t where
  assoc = isomorphismFS assoc

instance {-# overlappable #-} (MonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
                              MonoidalCategory (FullSubcategory ob c) t where
  leftIdentity = isomorphismFS leftIdentity
  rightIdentity = isomorphismFS rightIdentity

instance {-# overlappable #-} (Semigroupoid c, Bifunctor c d e t, BOb cOb dOb eOb t) =>
         Bifunctor (FullSubcategory cOb c) (FullSubcategory dOb d) (FullSubcategory eOb e) t where
  bimap f g = FS (bimap (inclusion f) (inclusion g))

instance (Semigroupoid c, Bifunctor (Opposite c) d e t, BOb cOb dOb eOb t) =>
         Bifunctor (Opposite (FullSubcategory cOb c)) (FullSubcategory dOb d) (FullSubcategory eOb e) t where
  bimap (Opposite f) g = FS (bimap (Opposite (inclusion f)) (inclusion g))

instance (c ~ (->)) =>
         SemigroupalCategory
         (FullSubcategory (Endofunctor c) (NaturalTransformation c))
         Compose where
  assoc =
    Iso
    (FS (NT (Compose . Compose . map getCompose . getCompose)))
    (FS (NT (Compose . map Compose . getCompose . getCompose)))

instance MonoidalCategory
         (FullSubcategory (Endofunctor (->)) (NaturalTransformation (->)))
         Compose where
  leftIdentity =
    Iso (FS (NT (runIdentity . getCompose))) (FS (NT (Compose . Identity)))
  rightIdentity =
    Iso
    (FS (NT (map runIdentity . getCompose)))
    (FS (NT (Compose . map Identity)))

instance (c ~ (->)) =>
         Bifunctor
         (FullSubcategory (Endofunctor c) (NaturalTransformation c))
         (FullSubcategory (Endofunctor c) (NaturalTransformation c))
         (FullSubcategory (Endofunctor c) (NaturalTransformation c))
         Compose where
  bimap f g =
    FS (NT (Compose . runNT (inclusion f) . map (runNT (inclusion g)) . getCompose))

-- | See, `Set.Set` /is/ a `Functor`.
instance Functor
         (FullSubcategory Base.Ord (->))
         (FullSubcategory Base.Ord (->))
         Set.Set where
  map = FS . Set.map . inclusion

-- | Like a `FullSubcategory`, this adds a constraint to a category, but it does
--   it without the possibility of nesting `FullSubcategory` constructors. I.e.,
--   you can't use `inclusion` on this to retrieve a previous less-constrained
--  `FullSubcategory`, but you avoid multiple unwrappings.
type family Constrained (ob :: ok -> Constraint) (c :: ok -> ok -> Type)
     :: ok -> ok -> Type where
  Constrained All c                       = c
  Constrained ob  (FullSubcategory ob' c) = Constrained (CFProd ob ob') c
  Constrained ob  (Opposite (FullSubcategory ob' c)) =
    Opposite (Constrained (CFProd ob ob') c)
  Constrained ob  c                       = FullSubcategory ob c

isomorphismFS :: Isomorphism c a b -> Isomorphism (FullSubcategory ob c) a b
isomorphismFS = Base.uncurry Iso . (FS . to &&& FS . from)

instance (HasTerminalObject c, ob (TerminalObject c)) =>
         HasTerminalObject (FullSubcategory ob c) where
  type TerminalObject (FullSubcategory ob c) = TerminalObject c
  (!) = FS (!)

instance (SemigroupalCategory (FullSubcategory ob c) t, ob a, Magma c t a) =>
         Magma (FullSubcategory ob c) t a where
  op = FS op

instance (SemigroupalCategory (FullSubcategory ob c) t, ob a, CommutativeMagma c t a) =>
         CommutativeMagma (FullSubcategory ob c) t a

instance (SemigroupalCategory (FullSubcategory ob c) t, ob a, IdempotentMagma c t a) =>
         IdempotentMagma (FullSubcategory ob c) t a

instance (SemigroupalCategory (FullSubcategory ob c) t, ob a, Semigroup c t a) =>
         Semigroup (FullSubcategory ob c) t a

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Unit c t), UnitalMagma c t a) =>
         UnitalMagma (FullSubcategory ob c) t a where
  unit t = FS (unit t)

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ComplementedLattice c t a) =>
         ComplementedLattice (FullSubcategory ob c) t a

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), UniquelyComplementedLattice c t a) =>
         UniquelyComplementedLattice (FullSubcategory ob c) t a where
  complement p = FS (complement p)

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ModularLattice c t a) =>
         ModularLattice (FullSubcategory ob c) t a

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), DistributiveLattice c t a) =>
         DistributiveLattice (FullSubcategory ob c) t a

instance (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), HeytingAlgebra c t a) =>
         HeytingAlgebra (FullSubcategory ob c) t a where
  implies = FS implies

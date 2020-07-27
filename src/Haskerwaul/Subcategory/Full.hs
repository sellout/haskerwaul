{-# language UndecidableInstances #-}

module Haskerwaul.Subcategory.Full
  ( module Haskerwaul.Subcategory.Full
  -- * extended modules
  , module Haskerwaul.Subcategory
  )where

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
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Constraint
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Complemented.Uniquely
import Haskerwaul.Object
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Subcategory
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/full+subcategory)
--
-- __NB__: This is probably less restricted than you think. E.g.,
--        @`FullSubcategory` (`Monoid` (->) (,)) (->)@ is /not/
--        [Mon(Hask)](https://ncatlab.org/nlab/show/category+of+monoids). The
--         objects are the same, but the morphisms are simply Haskell functions,
--        /not/ monoid homomorphisms.
newtype FullSubcategory (ob :: ok -> Constraint) (c :: ok -> ok -> Type) a b =
  FS (a `c` b)

type instance Ob (FullSubcategory ob c) = CFProd (Ob c) ob

instance Subcategory (FullSubcategory ob c) c where
  inclusion (FS f) = f

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
  bimap (FS f) (FS g) = FS (bimap f g)

instance (Semigroupoid c, Bifunctor (Opposite c) d e t, BOb cOb dOb eOb t) =>
         Bifunctor (Opposite (FullSubcategory cOb c)) (FullSubcategory dOb d) (FullSubcategory eOb e) t where
  bimap (Opposite (FS f)) (FS g) = FS (bimap (Opposite f) g)

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
  bimap (FS f) (FS g) = FS (NT (Compose . runNT f . map (runNT g) . getCompose))

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
  Constrained ob  (Opposite c)            = Opposite (Constrained ob c)
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

instance (BraidedMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
         BraidedMonoidalCategory (FullSubcategory ob c) t where
  braid = FS braid

instance (SymmetricMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
         SymmetricMonoidalCategory (FullSubcategory ob c) t

-- | If @c@ is Cartesian monoidal and the requisite objects are in the
--   subcategory, then the subcategory is also Cartesian monoidal.
instance (CartesianMonoidalCategory c, TOb ob (Prod c), ob (TerminalObject c)) =>
         CartesianMonoidalCategory (FullSubcategory ob c) where
  type Prod (FullSubcategory ob c) = Prod c
  exl = FS exl
  exr = FS exr
  diagonal = FS diagonal

-- instance CartesianMonoidalCategory (FullSubcategory (Monoid (->) (,)) (Opposite (->))) where
--   type Prod (FullSubcategory (Monoid (->) (,)) (Opposite (->))) = (,)
--   exl = FS (Opposite (, id))
--   exr = FS (Opposite (id, ))

-- | If @c@ is closed and the requisite objects are in the subcategory then the
--   subcategory is also closed monoidal.
instance (ClosedCategory c, TOb ob (Exp c)) =>
         ClosedCategory (FullSubcategory ob c) where
  type Exp (FullSubcategory ob c) = Exp c

-- | If @c@ is closed monoidal and the requisite objects are in the subcategory
--   then the subcategory is also closed monoidal.
instance (ClosedMonoidalCategory c t, TOb ob t, ob (Unit c t), TOb ob (Exp c)) =>
         ClosedMonoidalCategory (FullSubcategory ob c) t where
  apply = FS apply

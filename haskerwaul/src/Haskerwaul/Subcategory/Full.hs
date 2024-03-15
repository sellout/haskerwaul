{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Subcategory.Full
  ( module Haskerwaul.Subcategory.Full,

    -- * extended modules
    module Haskerwaul.Subcategory,
  )
where

import Control.Arrow ((&&&))
import Data.Constraint.Deferrable ((:~:) (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import qualified Data.Ord as Base
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import qualified Data.Tuple as Base
import Haskerwaul.Algebra.Heyting
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Complemented.Uniquely
import Haskerwaul.Object
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Subcategory
import Haskerwaul.Topos.Elementary
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/full+subcategory)
--
-- __NB__: This is probably less restricted than you think. E.g.,
--        @`FullSubcategory` (`Monoid` (->) (,)) (->)@ is /not/
--        [Mon(Hask)](https://ncatlab.org/nlab/show/category+of+monoids). The
--         objects are the same, but the morphisms are simply Haskell functions,
--        /not/ monoid homomorphisms.
newtype FullSubcategory (ob :: ok -> Constraint) (c :: ok -> ok -> Type) a b = FS {runFS :: a `c` b}

type instance Ob (FullSubcategory ob c) = CFProd (Ob c) ob

instance Subcategory (FullSubcategory ob c) c where
  inclusion (FS f) = f

instance
  (Magma (DinaturalTransformation (->)) Procompose c) =>
  Magma (DinaturalTransformation (->)) Procompose (FullSubcategory ob c)
  where
  op = DT (\(Procompose (FS f) (FS g)) -> FS (f . g))

instance
  (Semigroup (DinaturalTransformation (->)) Procompose c) =>
  Semigroup (DinaturalTransformation (->)) Procompose (FullSubcategory ob c)

instance
  (MonoidalCategory' c t, ob (Unit c t)) =>
  MonoidalCategory' (FullSubcategory ob c) t
  where
  type Unit (FullSubcategory ob c) t = Unit c t

instance
  (UnitalMagma (DinaturalTransformation (->)) Procompose c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (FullSubcategory ob c)
  where
  unit Proxy = DT (\Refl -> FS id)

instance
  (LeftQuasigroup (DinaturalTransformation (->)) Procompose c) =>
  LeftQuasigroup
    (DinaturalTransformation (->))
    Procompose
    (FullSubcategory ob c)
  where
  leftQuotient = DT FS . leftQuotient . bimap (DT inclusion) (DT inclusion)

instance
  (RightQuasigroup (DinaturalTransformation (->)) Procompose c) =>
  RightQuasigroup
    (DinaturalTransformation (->))
    Procompose
    (FullSubcategory ob c)
  where
  rightQuotient = DT FS . rightQuotient . bimap (DT inclusion) (DT inclusion)

instance
  {-# OVERLAPPABLE #-}
  (SemigroupalCategory c t, TOb ob t) =>
  SemigroupalCategory (FullSubcategory ob c) t
  where
  assoc = isomorphismFS assoc

instance
  {-# OVERLAPPABLE #-}
  (MonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
  MonoidalCategory (FullSubcategory ob c) t
  where
  leftIdentity = isomorphismFS leftIdentity
  rightIdentity = isomorphismFS rightIdentity

instance
  {-# OVERLAPPABLE #-}
  (Semigroupoid c, Bifunctor c d e t, BOb cOb dOb eOb t) =>
  Bifunctor (FullSubcategory cOb c) (FullSubcategory dOb d) (FullSubcategory eOb e) t
  where
  bimap (FS f) (FS g) = FS (bimap f g)

instance
  (Semigroupoid c, Bifunctor (Opposite c) d e t, BOb cOb dOb eOb t) =>
  Bifunctor (Opposite (FullSubcategory cOb c)) (FullSubcategory dOb d) (FullSubcategory eOb e) t
  where
  bimap (Opposite (FS f)) (FS g) = FS (bimap (Opposite f) g)

instance
  (c ~ (->)) =>
  SemigroupalCategory
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    Compose
  where
  assoc =
    Iso
      (FS (NT (Compose . Compose . map getCompose . getCompose)))
      (FS (NT (Compose . map Compose . getCompose . getCompose)))

instance
  MonoidalCategory
    (FullSubcategory (Endofunctor (->)) (NaturalTransformation (->) (->)))
    Compose
  where
  leftIdentity =
    Iso (FS (NT (runIdentity . getCompose))) (FS (NT (Compose . Identity)))
  rightIdentity =
    Iso
      (FS (NT (map runIdentity . getCompose)))
      (FS (NT (Compose . map Identity)))

instance
  (c ~ (->)) =>
  Bifunctor
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    Compose
  where
  bimap (FS f) (FS g) = FS (NT (Compose . runNT f . map (runNT g) . getCompose))

-- | See, `Set.Set` /is/ a `Functor`.
instance
  Functor
    (FullSubcategory Base.Ord (->))
    (FullSubcategory Base.Ord (->))
    Set.Set
  where
  map = FS . Set.map . inclusion

-- | Like a `FullSubcategory`, this adds a constraint to a category, but it does
--   it without the possibility of nesting `FullSubcategory` constructors. I.e.,
--   you can't use `inclusion` on this to retrieve a previous less-constrained
--  `FullSubcategory`, but you avoid multiple unwrappings.
type family
  Constrained (ob :: ok -> Constraint) (c :: ok -> ok -> Type) ::
    ok -> ok -> Type
  where
  Constrained All c = c
  Constrained ob (FullSubcategory ob' c) = Constrained (CFProd ob ob') c
  Constrained ob (Opposite c) = Opposite (Constrained ob c)
  Constrained ob c = FullSubcategory ob c

isomorphismFS :: Isomorphism c a b -> Isomorphism (FullSubcategory ob c) a b
isomorphismFS = Base.uncurry Iso . (FS . to &&& FS . from)

instance
  (HasTerminalObject c, ob (TerminalObject c)) =>
  HasTerminalObject (FullSubcategory ob c)
  where
  type TerminalObject (FullSubcategory ob c) = TerminalObject c
  (!) = FS (!)

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Magma c t a) =>
  Magma (FullSubcategory ob c) t a
  where
  op = FS op

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Magma (Opposite c) t a) =>
  Magma (Opposite (FullSubcategory ob c)) t a
  where
  op = Opposite (FS (opposite op))

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, CommutativeMagma c t a) =>
  CommutativeMagma (FullSubcategory ob c) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, CommutativeMagma (Opposite c) t a) =>
  CommutativeMagma (Opposite (FullSubcategory ob c)) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, LeftShelf c t a) =>
  LeftShelf (FullSubcategory ob c) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, LeftShelf (Opposite c) t a) =>
  LeftShelf (Opposite (FullSubcategory ob c)) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Band c t a) =>
  Band (FullSubcategory ob c) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Band (Opposite c) t a) =>
  Band (Opposite (FullSubcategory ob c)) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, LeftRegularBand c t a) =>
  LeftRegularBand (FullSubcategory ob c) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, LeftRegularBand (Opposite c) t a) =>
  LeftRegularBand (Opposite (FullSubcategory ob c)) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Semigroup c t a) =>
  Semigroup (FullSubcategory ob c) t a

instance
  (SemigroupalCategory (FullSubcategory ob c) t, ob a, Semigroup (Opposite c) t a) =>
  Semigroup (Opposite (FullSubcategory ob c)) t a

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Unit c t), UnitalMagma c t a) =>
  UnitalMagma (FullSubcategory ob c) t a
  where
  unit t = FS (unit t)

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Unit c t), UnitalMagma (Opposite c) t a) =>
  UnitalMagma (Opposite (FullSubcategory ob c)) t a
  where
  unit t = Opposite (FS (opposite (unit t)))

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ob (Unit c t), ComplementedLattice c t a) =>
  ComplementedLattice (FullSubcategory ob c) t a

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ob (Unit c t), UniquelyComplementedLattice c t a) =>
  UniquelyComplementedLattice (FullSubcategory ob c) t a
  where
  complement p = FS (complement p)

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ModularLattice c t a) =>
  ModularLattice (FullSubcategory ob c) t a

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), DistributiveLattice c t a) =>
  DistributiveLattice (FullSubcategory ob c) t a

instance
  (MonoidalCategory (FullSubcategory ob c) t, ob a, ob (Meet a), ob (Join a), ob (Unit c t), HeytingAlgebra c t a) =>
  HeytingAlgebra (FullSubcategory ob c) t a
  where
  implies = FS implies

instance
  (BraidedMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
  BraidedMonoidalCategory (FullSubcategory ob c) t
  where
  braid = Iso (FS (to braid)) (FS (from braid))

instance
  (BalancedMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
  BalancedMonoidalCategory (FullSubcategory ob c) t
  where
  balance = FS . balance

instance
  (SymmetricMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
  SymmetricMonoidalCategory (FullSubcategory ob c) t

-- | If @c@ is Cartesian monoidal and the requisite objects are in the
--   subcategory, then the subcategory is also Cartesian monoidal.
instance
  (CartesianMonoidalCategory c, TOb ob (Prod c), ob (TerminalObject c)) =>
  CartesianMonoidalCategory (FullSubcategory ob c)
  where
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
instance
  (ClosedCategory c, TOb ob (InternalHom c)) =>
  ClosedCategory (FullSubcategory ob c)
  where
  type InternalHom (FullSubcategory ob c) = InternalHom c

-- | If @c@ is closed monoidal and the requisite objects are in the subcategory
--   then the subcategory is also closed monoidal.
instance
  (ClosedMonoidalCategory c t, TOb ob t, ob (Unit c t), TOb ob (InternalHom c)) =>
  ClosedMonoidalCategory (FullSubcategory ob c) t
  where
  apply = FS apply
  curry (FS f) = FS (curry f)

instance
  (CartesianClosedCategory c, ob (Unit c (Prod c)), TOb ob (InternalHom c)) =>
  CartesianClosedCategory (FullSubcategory ob c)
  where
  const = FS const
  flattenHom = FS flattenHom

instance
  ( CartesianClosedMonoidalCategory c,
    TOb ob (Prod c),
    ob (TerminalObject c),
    TOb ob (InternalHom c)
  ) =>
  CartesianClosedMonoidalCategory (FullSubcategory ob c)
  where
  tuple = FS tuple

instance
  ( ElementaryTopos c,
    TOb ob (Prod c),
    ob (TerminalObject c),
    TOb ob (InternalHom c),
    ob (Class c),
    ob (Meet (Class c)),
    ob (Join (Class c))
  ) =>
  ElementaryTopos (FullSubcategory ob c)
  where
  type Class (FullSubcategory ob c) = Class c
  true = FS true

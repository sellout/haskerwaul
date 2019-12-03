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

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Monoidal
import Haskerwaul.Constraint
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Transformation.Natural

-- | https://ncatlab.org/nlab/show/full+subcategory
--
--  `Haskerwaul.Magma.Unital.unit` (on objects) and `inclusion` (on morphisms)
--   form an <https://ncatlab.org/nlab/show/subcategory inclusion functor>.
--
-- __NB__: This is probably less restrictive than you think. E.g.,
--        `FullSubcategory (Monoid (->) (,)) (->)` is /not/
--        <https://ncatlab.org/nlab/show/category+of+monoids Mon(Hask)>. The
--         objects are the same, but the morphisms are simply Haskell functions,
--        /not/ monoid homomorphisms.
newtype FullSubcategory (c :: ok -> Constraint) (k :: ok -> ok -> Type) a b =
  FS { inclusion :: a `k` b }

type instance Ob (FullSubcategory c k) = CFProd (Ob k) c

instance Magma (NaturalTransformation2 (->)) CProd k =>
         Magma (NaturalTransformation2 (->)) CProd (FullSubcategory c k) where
  op = NT2 (\(CProd (FS f) (FS g)) -> FS (f . g))

instance Semigroup (NaturalTransformation2 (->)) CProd k =>
         Semigroup (NaturalTransformation2 (->)) CProd (FullSubcategory c k)

instance (MonoidalCategory' k t, c (Unit k t)) =>
         MonoidalCategory' (FullSubcategory c k) t where
  type Unit (FullSubcategory c k) t = Unit k t

instance UnitalMagma (NaturalTransformation2 (->)) CProd k =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (FullSubcategory c k) where
  unit Proxy = NT2 (\Refl -> FS id)

instance {-# overlappable #-} (SemigroupalCategory c t, TOb ob t) =>
                              SemigroupalCategory (FullSubcategory ob c) t where
  assoc = isomorphismFS assoc

instance {-# overlappable #-} (MonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
                              MonoidalCategory (FullSubcategory ob c) t where
  leftIdentity = isomorphismFS leftIdentity
  rightIdentity = isomorphismFS rightIdentity

instance (Semigroupoid c, Bifunctor c d e t, BOb cOb dOb eOb t) =>
         Bifunctor (FullSubcategory cOb c) (FullSubcategory dOb d) (FullSubcategory eOb e) t where
  bimap f g = FS (bimap (inclusion f) (inclusion g))

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

isomorphismFS :: Isomorphism k a b -> Isomorphism (FullSubcategory c k) a b
isomorphismFS = Base.uncurry Iso . (FS . to &&& FS . from)

instance (HasTerminalObject c, ob (TerminalObject c)) =>
         HasTerminalObject (FullSubcategory ob c) where
  type TerminalObject (FullSubcategory ob c) = TerminalObject c
  (!) = FS (!)

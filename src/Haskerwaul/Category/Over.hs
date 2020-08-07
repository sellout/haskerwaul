{-# language UndecidableInstances #-}

module Haskerwaul.Category.Over where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy (..))

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Topos.Elementary
import Haskerwaul.Transformation.Dinatural

-- | In our representation of an over (or slice) category __c/x__, the objects
--   are /represented by/ the objects of the underlying category, but the terms
--   are still all morphisms.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/over+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Comma_category#Slice_category)
newtype Over (c :: ok -> ok -> *) (x :: ok) a b = Over (a `c` x -> b `c` x)

type instance Ob (Over c x) = Ob c

instance Semigroupoid c => Magma (DinaturalTransformation (->)) Procompose (Over c x) where
  op = DT (\(Procompose (Over f) (Over g)) -> Over (f . g))

instance (Semigroupoid c, Ob c x) => Semigroup (DinaturalTransformation (->)) Procompose (Over c x)

instance (Category c, Ob c x) => UnitalMagma (DinaturalTransformation (->)) Procompose (Over c x) where
  unit Proxy = DT (\Refl -> Over id)

-- instance Bifunctor c c c t => Bifunctor (Over c x) (Over c x) (Over c x) t where
  

-- instance (SemigroupalCategory c t, Ob c x) => SemigroupalCategory (Over c x) t where
--   -- assoc = Iso (Over (to assoc)) (Over (from assoc))

-- instance (CartesianClosedCategory c, t ~ Prod c, Ob c x) => MonoidalCategory' (Over c x) t where
--   type Unit (Over c x) t = Prod c (Unit c t) x

-- instance (MonoidalCategory c t, Ob c x) => MonoidalCategory (Over c x) t where
--   -- leftIdentity = Iso (Over (first Proxy (to leftIdentity))) (Over (first Proxy (from leftIdentity)))
--   -- rightIdentity = Iso (Over (first Proxy (to rightIdentity))) (Over (first Proxy (from rightIdentity)))

-- instance (BraidedMonoidalCategory c t, Ob c x) => BraidedMonoidalCategory (Over c x) t

-- instance (SymmetricMonoidalCategory c t, Ob c x) => SymmetricMonoidalCategory (Over c x) t

-- instance (HasTerminalObject c, Ob c x) => HasTerminalObject (Over c x) where
--   type TerminalObject (Over c x) = Prod c (TerminalObject c) x

-- instance (ClosedCategory c, Ob c x)  => ClosedCategory (Over c x) where
-- --  type Exp (Over c x) = Prod c (Exp c) x

-- instance (ClosedMonoidalCategory c t, Ob c x)  => ClosedMonoidalCategory (Over c x) t

-- instance (CartesianMonoidalCategory c, Ob c x) => CartesianMonoidalCategory (Over c x) where
--   type Prod (Over c x) = Prod c

-- instance (CartesianClosedCategory c, Ob c x) => CartesianClosedCategory (Over c x) where

  

-- -- | [nLab](https://ncatlab.org/nlab/show/over-topos)
-- instance (ElementaryTopos c, Ob c x) => ElementaryTopos (Over c x) where
--   type Class (Over c x) = Prod c (Class c) x
-- --  true = 

module Haskerwaul.Category.Opposite where

import           Control.Arrow ((&&&))
import           Data.Constraint ((:-)(..), Bottom(..), bottom)
import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Tuple as Base
import qualified Data.Void as Base

import Haskerwaul.Category
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/opposite+category)
newtype Opposite c a b = Opposite { opposite :: b `c` a }

type instance Ob (Opposite c) = Ob c

instance Magma (NaturalTransformation2 (->)) CProd c =>
         Magma (NaturalTransformation2 (->)) CProd (Opposite c) where
  op = NT2 (\(CProd (Opposite f) (Opposite g)) -> Opposite (g . f))

instance Semigroup (NaturalTransformation2 (->)) CProd c =>
         Semigroup (NaturalTransformation2 (->)) CProd (Opposite c)

instance MonoidalCategory' c t => MonoidalCategory' (Opposite c) t where
  type Unit (Opposite c) t = Unit c t

instance UnitalMagma (NaturalTransformation2 (->)) CProd c =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (Opposite c) where
  unit Proxy = NT2 (\Refl -> Opposite id)

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
  -- Op (FullSubcategory ob (Opposite c)) = Constrained ob c
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

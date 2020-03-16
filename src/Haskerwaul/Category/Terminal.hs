module Haskerwaul.Category.Terminal where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Pointed
import Haskerwaul.Constraint
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/terminal+category)
--
--  __NB__: This appears to have the same objects as @(->)@, but in this
--          category, they're all isomorphic.
data TerminalCategory a b = TermId

-- | There is a unique functor from any category @c@ to the
--  `TerminalCategory`. As mentioned in "Haskerwaul.Functor", this can't be
--   implemented as a `Functor` instance.
toTerminal :: a `c` b -> TerminalCategory a b
toTerminal _ = TermId

type instance Ob TerminalCategory = All

-- | All objects in this category are isomorphic.
terminalIso :: Isomorphism TerminalCategory a b
terminalIso = Iso TermId TermId

instance Magma (NaturalTransformation2 (->)) CProd TerminalCategory where
  op = NT2 (\(CProd _ _) -> TermId)

instance Semigroup (NaturalTransformation2 (->)) CProd TerminalCategory

instance UnitalMagma (NaturalTransformation2 (->)) CProd TerminalCategory where
  unit Proxy = NT2 (\Refl -> TermId)

-- | Every object is a tensor.
instance SemigroupalCategory TerminalCategory t where
  assoc = terminalIso

instance MonoidalCategory' (TerminalCategory :: Type -> Type -> Type) t where
  -- | Doesn't matter what type we use here, as they're all isomorphic.
  type Unit TerminalCategory t = ()

instance MonoidalCategory (TerminalCategory :: Type -> Type -> Type) t where
  leftIdentity = terminalIso
  rightIdentity = terminalIso

instance HasTerminalObject (TerminalCategory :: Type -> Type -> Type) where
  -- | Doesn't matter what type we use here, as they're all isomorphic.
  type TerminalObject TerminalCategory = ()
  (!) = TermId

instance HasTerminalObject (Opposite (TerminalCategory :: Type -> Type -> Type)) where
  -- | Doesn't matter what type we use here, as they're all isomorphic.
  type TerminalObject (Opposite TerminalCategory) = ()
  (!) = Opposite TermId

instance PointedCategory (TerminalCategory :: Type -> Type -> Type) where
  (!-) = TermId

-- | There is a unique functor @c -> `TerminalCategory`@ for each category @c@.
instance Functor c TerminalCategory f where
  map _ = TermId

instance Bifunctor c1 c2 TerminalCategory f where
  bimap _ _ = TermId

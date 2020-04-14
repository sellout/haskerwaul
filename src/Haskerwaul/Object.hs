{-# language TypeApplications #-}

module Haskerwaul.Object where

import           Data.Constraint ((:-)(..), Dict(..), (\\))
import           Data.Kind (Constraint, Type)
import qualified Data.Ord as Base
import qualified Data.Set as Set
import           Data.Type.Equality ((:~:))

import Haskerwaul.Constraint

-- | The objects in the category. This allows us to restrict them more than
--   the kind alone would do. By default it is the empty constraint, and
--   should only be changed in cases where the constraint is lifted to a type
--   parameter (as in `Haskerwaul.Subcategory.Full.FullSubcategory`).
--
-- __NB__: Ideally this would be an associated type on
--        `Haskerwaul.Semigroupoid.Semigroupoid`, but [type families can't
--         overlap](https://gitlab.haskell.org/ghc/ghc/issues/4259), so we
--         extract it here and remove the @overlappable@ case in favor of more
--         specific ones.
type family Ob (c :: ok -> ok -> Type) :: ok -> Constraint

type instance Ob (->) = All

type instance Ob (:-) = All

type instance Ob (:~:) = All

-- | An `Ob` in a functor category.
class FOb cOb dOb f where
  inF :: cOb x :- dOb (f x)

instance FOb cOb All f where
  inF = Sub Dict

instance (FOb cOb dOb f, FOb cOb' dOb' f) =>
         FOb (CFProd cOb cOb') (CFProd dOb dOb') f where
  inF :: forall x. CFProd cOb cOb' x :- CFProd dOb dOb' (f x)
  inF = Sub (Dict \\ inF @cOb @dOb @f @x \\ inF @cOb' @dOb' @f @x)

instance FOb Base.Ord Base.Ord Set.Set where
  inF = Sub Dict

-- | An `Ob` in a bifunctor category.
class BOb cOb dOb eOb b where
  inB :: (cOb x, dOb y) :- eOb (b x y)

instance BOb cOb dOb All t where
  inB = Sub Dict

instance (BOb cOb dOb eOb b, BOb cOb' dOb' eOb' b) =>
         BOb (CFProd cOb cOb') (CFProd dOb dOb') (CFProd eOb eOb') b where
  inB :: forall x y. (CFProd cOb cOb' x, CFProd dOb dOb' y) :- CFProd eOb eOb' (b x y)
  inB = Sub (Dict \\ inB @cOb @dOb @eOb @b @x @y \\ inB @cOb' @dOb' @eOb' @b @x @y)

-- instance BOb cOb dOb eOb c => BOb dOb cOb eOb (Opposite c) where
--   inB :: forall x y. (cOb x, dOb y, BOb cOb dOb eOb (c x y)) :- BOb dOb cOb eOb (Op c x y)
--   inB = Sub (Dict \\ inB @cOb @dOb @eOb @c @x @y)

-- | An `Ob` for a tensor.
type TOb cOb = BOb cOb cOb cOb

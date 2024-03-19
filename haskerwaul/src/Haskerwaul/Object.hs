{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Object where

import Data.Constraint (Dict (..), (:-) (..), (\\))
import Data.Kind (Constraint, Type)
import qualified Data.Ord as Base
import qualified Data.Set as Set
import Data.Type.Equality ((:~:))
import Haskerwaul.Constraint
import Prelude (Either, Eq, Show)

-- | The objects in the category. This allows us to restrict them more than
--   the kind alone would do. By default it is the empty constraint, and
--   should only be changed in cases where the constraint is lifted to a type
--   parameter (as in `Haskerwaul.Subcategory.Full.FullSubcategory`).
--
-- __NB__: Ideally this would be an associated type on
--        `Haskerwaul.Semicategory.Semicategory`, but [type families can't
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

instance
  (FOb cOb dOb f, FOb cOb' dOb' f) =>
  FOb (CFProd cOb cOb') (CFProd dOb dOb') f
  where
  inF :: forall x. CFProd cOb cOb' x :- CFProd dOb dOb' (f x)
  inF = Sub (Dict \\ inF @cOb @dOb @f @x \\ inF @cOb' @dOb' @f @x)

instance FOb Base.Ord Base.Ord Set.Set where
  inF = Sub Dict

-- | An `Ob` in a bifunctor category.
class BOb cOb dOb eOb b where
  inB :: (cOb x, dOb y) :- eOb (b x y)

instance BOb cOb dOb All t where
  inB = Sub Dict

instance BOb (FOb cOb dOb) (FOb eOb fOb) (FOb gOb All) t where
  inB = Sub Dict

instance BOb (BOb cOb dOb eOb) (BOb fOb gOb hOb) (BOb iOb jOb All) t where
  inB = Sub Dict

instance {-# OVERLAPPABLE #-} (BOb All All eOb t) => BOb cOb dOb eOb t where
  inB = inB

instance
  (BOb cOb dOb eOb b, BOb cOb' dOb' eOb' b) =>
  BOb (CFProd cOb cOb') (CFProd dOb dOb') (CFProd eOb eOb') b
  where
  inB :: forall x y. (CFProd cOb cOb' x, CFProd dOb dOb' y) :- CFProd eOb eOb' (b x y)
  inB = Sub (Dict \\ inB @cOb @dOb @eOb @b @x @y \\ inB @cOb' @dOb' @eOb' @b @x @y)

-- | An `Ob` for a tensor.
type TOb cOb = BOb cOb cOb cOb

inT :: (TOb cOb t) => (cOb x, cOb y) :- cOb (t x y)
inT = inB

-- Instances that would be orphan anywhere else, but are only needed for
-- testing, since Hedgehog requires them.

instance BOb Eq Eq Eq (,) where
  inB = Sub Dict

instance BOb Eq Eq Eq Either where
  inB = Sub Dict

instance BOb Show Show Show (,) where
  inB = Sub Dict

instance BOb Show Show Show Either where
  inB = Sub Dict

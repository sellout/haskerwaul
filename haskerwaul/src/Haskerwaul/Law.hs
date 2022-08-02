{-# LANGUAGE Safe #-}
-- __NB__: The @rel c y@ constraint on `checkLaw` is technically redundant, but
--         constraints-0.14 might allow us to specify the relationship between
--        `HomogeneousRelation'` and @rel@ so that it isn’t.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Defining laws in a category-polymorphic way is tricky.
module Haskerwaul.Law where

import Data.Kind (Constraint, Type)
import Haskerwaul.Bifunctor
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Topos.Elementary

-- | A law is a pair of morphisms with a relation constraint.
--
--   The relation is managed at the type level because we define laws in
--   arbitrary categories (or even semicategories), but can only validate them
--   in a topos. When a law is defined, we don't know which topos(es) it may be
--   tested in, so the constraint allows us to concretize the comparison after
--   we've chosen a topos for testing.
--
--   The @relation@ should be a subclass of `HomogeneousRelation`. This isn't
--  (yet) enforced anywhere, but if it's not the case, your @relation@ will be
--   redundant and an arbitrary `HomogeneousRelation` will be found and used.
--
--  __NB__: For the purposes of reporting, we treat the first argument as the
--         "expectation" and the second as the "actual". This means that the
--          first should generally be the simpler one that doesn't necessarily
--          use all the operations involved. However, if you have a @relation@
--          that isn't reflexive, you don't get to choose which side is the
--          simpler one.
data Law c (relation :: (Type -> Type -> Type) -> Type -> Constraint) a b = Law
  { expectation :: a `c` b,
    actual :: a `c` b
  }

-- | Builds a [characteristic
--   morphism](https://ncatlab.org/nlab/show/characteristic+function#of_a_subobject)
--   for testing a `Law`.
--
--   The first argument, @translate@, should be a category homomorphism,
--   bringing us from the category we're testing to the topos we can validate
--   within. If this is not a homomorphism, all bets are off for your test doing
--   anything reasonable. The most trivial case is when you are already in the
--   topos, then you can use `id`. Otherwise, you may have various newtype
--   wrappers and unwrappers, like `NT`/`runNT`, `Opposite`/`opposite`, etc. and
--   they can generally be composed to move across vast spaces of __Cat__.
checkLaw ::
  (ElementaryTopos c, relation c y, HomogeneousRelation' c y, Ob c x) =>
  (a `d` b -> x `c` y) ->
  Law d relation a b ->
  x `c` Class c
checkLaw translate (Law exp act) =
  rel . bimap (translate exp) (translate act) . diagonal

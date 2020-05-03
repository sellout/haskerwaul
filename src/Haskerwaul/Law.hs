-- | Defining laws in a category-polymorphic way is tricky.
module Haskerwaul.Law where

import Haskerwaul.Bifunctor
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

-- | A law is a pair of morphisms that should be equivalent.
--
--  __NB__: Since it's simply a test of equivalence, it doesn't matter which
--          morphism is which, but for the purposes of reporting, we treat the
--          first argument as the "expectation" and the second as the
--         "actual". This means that the first should generally be the simpler
--          one that doesn't necessarily use all the operations involved.
data Law c a b = Law (a `c` b) (a `c` b)

-- | Builds a [characteristic morphism](https://ncatlab.org/nlab/show/characteristic+function#of_a_subobject)
--   for testing a `Law`.
--
--  __TODO__: Regardless of what the constraints on the `Law` are, we need an
--           `ElementaryTopos` in order to have some way to evaluate the result.
--            This seems overly restrictive.
checkLaw :: (ElementaryTopos c, Ob c a, Ob c b)
         => Law c a b -> BinaryRelation c b b -> a `c` Class c
checkLaw (Law x y) eq = eq . bimap x y . diagonal

-- | Defining laws in a category-polymorphic way is tricky.
module Haskerwaul.Law where

import Haskerwaul.Bifunctor
import Haskerwaul.Object
import Haskerwaul.Topos.Elementary

type EquivalenceRelation c a = Prod c a a `c` Class c

-- | A law is a pair of morphisms that should be equivalent.
data Law c a b = Law (a `c` b) (a `c` b)

checkLaw :: (ElementaryTopos c, Ob c a, Ob c b, Bifunctor c c c (Prod c))
         => Law c a b -> EquivalenceRelation c b -> a `c` Class c
checkLaw (Law x y) eq = eq . bimap x y . duplicate

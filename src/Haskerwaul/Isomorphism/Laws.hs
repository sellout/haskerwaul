module Haskerwaul.Isomorphism.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

isomorphismLaw :: (Category c, Ob c a, Ob c b) => Isomorphism c a b -> Law c a a
isomorphismLaw iso = Law id (from iso . to iso)

inverseIsomorphismLaw :: (Category c, Ob c a, Ob c b) => Isomorphism c a b -> Law c b b
inverseIsomorphismLaw iso = Law id (to iso . from iso)

data IsomorphismLaws c a b =
  IsomorphismLaws
    { forward :: a `c` Class c
    , backward :: b `c` Class c
    }

isomorphismLaws :: (ElementaryTopos c, Ob c a, Ob c b)
                => Isomorphism c a b -> BinaryRelation c a a -> BinaryRelation c b b -> IsomorphismLaws c a b
isomorphismLaws iso eqA eqB =
  IsomorphismLaws
    { forward = checkLaw (isomorphismLaw iso) eqA
    , backward = checkLaw (inverseIsomorphismLaw iso) eqB
    }

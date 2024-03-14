module Haskerwaul.Isomorphism.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

isomorphismLaw :: (Category c, Ob c a, Ob c b) => Isomorphism c a b -> Law c EqualityRelation a a
isomorphismLaw iso = Law id (from iso . to iso)

inverseIsomorphismLaw ::
  (Category c, Ob c a, Ob c b) => Isomorphism c a b -> Law c EqualityRelation b b
inverseIsomorphismLaw iso = Law id (to iso . from iso)

data IsomorphismLaws c a b = IsomorphismLaws
  { forward :: Law c EqualityRelation a a,
    backward :: Law c EqualityRelation b b
  }

isomorphismLaws ::
  (Category c, Ob c a, Ob c b) => Isomorphism c a b -> IsomorphismLaws c a b
isomorphismLaws iso =
  IsomorphismLaws
    { forward = isomorphismLaw iso,
      backward = inverseIsomorphismLaw iso
    }

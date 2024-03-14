module Haskerwaul.Law.Commutativity where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

commutativity :: (BraidedMonoidalCategory c t, Ob c a, Ob c b)
              => t a a `c` b -> Law c EqualityRelation (t a a) b
commutativity op' = Law op' (op' . to braid)

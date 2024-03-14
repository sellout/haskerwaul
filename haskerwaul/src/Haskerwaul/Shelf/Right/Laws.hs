module Haskerwaul.Shelf.Right.Laws where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Law.Distributive.Self.Right
import Haskerwaul.Relation.Equality
import Haskerwaul.Shelf.Right

data RightShelfLaws c a = RightShelfLaws
  { rightSelfDistributive :: Law c EqualityRelation (Prod c (Prod c a a) a) a
  }

rightShelfLaws ::
  (CartesianMonoidalCategory c, RightShelf c (Prod c) a) => RightShelfLaws c a
rightShelfLaws =
  RightShelfLaws
    { rightSelfDistributive = rightSelfDistributiveLaw op
    }

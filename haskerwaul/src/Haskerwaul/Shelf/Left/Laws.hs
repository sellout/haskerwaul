module Haskerwaul.Shelf.Left.Laws where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Law.Distributive.Self.Left
import Haskerwaul.Relation.Equality
import Haskerwaul.Shelf.Left

data LeftShelfLaws c a = LeftShelfLaws
  { leftSelfDistributive :: Law c EqualityRelation (Prod c a (Prod c a a)) a
  }

leftShelfLaws ::
  (CartesianMonoidalCategory c, LeftShelf c (Prod c) a) => LeftShelfLaws c a
leftShelfLaws =
  LeftShelfLaws
    { leftSelfDistributive = leftSelfDistributiveLaw op
    }

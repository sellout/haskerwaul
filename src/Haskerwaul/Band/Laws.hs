module Haskerwaul.Band.Laws where

import Haskerwaul.Band
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Law.Idempotency
import Haskerwaul.Relation.Equality
import Haskerwaul.Semigroup.Laws

data BandLaws c a =
  BandLaws
    { semigroup :: SemigroupLaws c (Prod c) a
    , idempotent :: Law c EqualityRelation a a
    }

bandLaws ::
  (CartesianMonoidalCategory c, Band c (Prod c) a) => BandLaws c a
bandLaws =
  BandLaws
    { semigroup = semigroupLaws
    , idempotent = idempotency op
    }

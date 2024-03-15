{-# LANGUAGE Safe #-}

module Haskerwaul.Semigroup.Laws where

import Haskerwaul.Law
import Haskerwaul.Law.Associativity
import Haskerwaul.Relation.Equality
import Haskerwaul.Topos.Elementary

data SemigroupLaws c t a = SemigroupLaws
  { associative :: Law c EqualityRelation (t (t a a) a) a
  }

semigroupLaws ::
  (SemigroupalCategory c t, Semigroup c t a) => SemigroupLaws c t a
semigroupLaws =
  SemigroupLaws
    { associative = associativity op
    }

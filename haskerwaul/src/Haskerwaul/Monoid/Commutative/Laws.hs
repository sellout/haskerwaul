{-# LANGUAGE Safe #-}

module Haskerwaul.Monoid.Commutative.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Monoid.Commutative
import Haskerwaul.Monoid.Laws
import Haskerwaul.Semigroup.Commutative.Laws

data CommutativeMonoidLaws c t a = CommutativeMonoidLaws
  { commutativeSemigroup :: CommutativeSemigroupLaws c t a,
    monoid :: MonoidLaws c t a
  }

commutativeMonoidLaws ::
  (BraidedMonoidalCategory c t, CommutativeMonoid c t a) =>
  CommutativeMonoidLaws c t a
commutativeMonoidLaws =
  CommutativeMonoidLaws
    { commutativeSemigroup = commutativeSemigroupLaws,
      monoid = monoidLaws
    }

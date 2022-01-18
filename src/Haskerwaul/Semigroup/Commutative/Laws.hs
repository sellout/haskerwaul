module Haskerwaul.Semigroup.Commutative.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Magma.Commutative.Laws
import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semigroup.Laws

data CommutativeSemigroupLaws c t a =
  CommutativeSemigroupLaws
    { commutativeMagma :: CommutativeMagmaLaws c t a
    , semigroup :: SemigroupLaws c t a
    }

commutativeSemigroupLaws ::
  (BraidedMonoidalCategory c t, CommutativeSemigroup c t a) =>
  CommutativeSemigroupLaws c t a
commutativeSemigroupLaws =
  CommutativeSemigroupLaws
    { commutativeMagma = commutativeMagmaLaws
    , semigroup = semigroupLaws
    }

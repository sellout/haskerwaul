{-# language RecordWildCards #-}

module Haskerwaul.Monoid.Commutative.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Magma.Commutative.Laws
import Haskerwaul.Monoid.Commutative
import Haskerwaul.Monoid.Laws

data CommutativeMonoidLaws c t a =
  CommutativeMonoidLaws
    { commutativeMagma :: CommutativeMagmaLaws c t a
    , monoid :: MonoidLaws c t a
    }

commutativeMonoidLaws ::
  (BraidedMonoidalCategory c t, CommutativeMonoid c t a) => CommutativeMonoidLaws c t a
commutativeMonoidLaws =
  CommutativeMonoidLaws
    { commutativeMagma = commutativeMagmaLaws
    , monoid = monoidLaws
    }

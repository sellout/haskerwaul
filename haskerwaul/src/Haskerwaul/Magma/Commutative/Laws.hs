{-# LANGUAGE Safe #-}

module Haskerwaul.Magma.Commutative.Laws where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law
import Haskerwaul.Law.Commutativity
import Haskerwaul.Magma.Commutative
import Haskerwaul.Relation.Equality

newtype CommutativeMagmaLaws c t a = CommutativeMagmaLaws
  { commutative :: Law c EqualityRelation (t a a) a
  }

commutativeMagmaLaws ::
  (BraidedMonoidalCategory c t, CommutativeMagma c t a) =>
  CommutativeMagmaLaws c t a
commutativeMagmaLaws = CommutativeMagmaLaws {commutative = commutativity op}

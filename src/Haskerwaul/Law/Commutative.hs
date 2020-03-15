module Haskerwaul.Law.Commutative where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law

commutativeLaw :: (BraidedMonoidalCategory c t, Magma c t a)
               => Law c (t a a) a
commutativeLaw = Law op (op . braid)

module Haskerwaul.Law.Commutative where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law
import Haskerwaul.Object

commutativeLaw :: (BraidedMonoidalCategory c t, Ob c a) => t a a `c` a -> Law c (t a a) a
commutativeLaw op' = Law op' (op' . braid)

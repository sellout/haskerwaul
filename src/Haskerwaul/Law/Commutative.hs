module Haskerwaul.Law.Commutative where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law
import Haskerwaul.Object

commutativeLaw :: (BraidedMonoidalCategory c t, Ob c a, Ob c b)
               => t a a `c` b -> Law c (t a a) b
commutativeLaw op' = Law op' (op' . braid)

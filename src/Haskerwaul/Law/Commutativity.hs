module Haskerwaul.Law.Commutativity where

import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law
import Haskerwaul.Object

commutativity :: (BraidedMonoidalCategory c t, Ob c a, Ob c b)
              => t a a `c` b -> Law c (t a a) b
commutativity op' = Law op' (op' . braid)

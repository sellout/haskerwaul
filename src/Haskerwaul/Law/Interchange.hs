{-# language TypeApplications #-}

module Haskerwaul.Law.Interchange where

import           Data.Constraint ((\\))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- |
-- = references
--
-- - [/Monoidal Functors, Species and Hopf Algebras/ §6](http://pi.math.cornell.edu/~maguiar/a.pdf)
interchangeLaw
  :: forall c di st a
   . (MonoidalCategory c di, MonoidalCategory c st, Monoid c di a, Monoid c st a)
  => (di (st a a) (st a a) `c` st (di a a) (di a a))
  -> Law c EqualityRelation (di (st a a) (st a a)) a
interchangeLaw inter =
  Law (op . bimap (op @c) (op @c)) (op . bimap (op @c) (op @c) . inter)
  \\ inT @(Ob c) @di @a @a
  \\ inT @(Ob c) @st @a @a

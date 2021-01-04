{-# language TypeApplications #-}

module Haskerwaul.Law.Interchange where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- |
-- = references
--
-- - [/Monoidal Functors, Species and Hopf Algebras/ §6](http://pi.math.cornell.edu/~maguiar/a.pdf)
interchangeLaw
  :: forall c t a. (BraidedMonoidalCategory c t, Ob c a)
  => t a a `c` a -> t a a `c` a -> Law c PartialOrder (t (t a a) (t a a)) a
interchangeLaw di st =
  Law
  (di . bimap st st)
  (st
   . bimap di di
   . to assoc
   . second @_ @c p (from assoc . first @c p braid . to assoc)
   . from assoc)
  \\ inT @(Ob c) @t @a @(t a a)
  \\ inT @(Ob c) @t @a @a
  where
    p = Proxy :: Proxy c

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Category.Semigroupal.Laws where

import Data.Constraint ((\\))
import Haskerwaul.Category.Laws
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Isomorphism.Laws
import Haskerwaul.Object

data SemigroupalCategoryLaws c t = SemigroupalCategoryLaws
  { category :: CategoryLaws c,
    associative ::
      forall x y z.
      (Ob c x, Ob c y, Ob c z) =>
      IsomorphismLaws c (t x (t y z)) (t (t x y) z)
  }

semigroupalCategoryLaws ::
  forall c t.
  (SemigroupalCategory c t) =>
  SemigroupalCategoryLaws c t
semigroupalCategoryLaws =
  SemigroupalCategoryLaws
    { category = categoryLaws,
      associative =
        isomorphismLaws assoc
          \\ inT @(Ob c) @t @x @(t y z)
          \\ inT @(Ob c) @t @(t x y) @z
          \\ inT @(Ob c) @t @x @y
          \\ inT @(Ob c) @t @y @z ::
          forall x y z.
          (Ob c x, Ob c y, Ob c z) =>
          IsomorphismLaws c (t x (t y z)) (t (t x y) z)
    }

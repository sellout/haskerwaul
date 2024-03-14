{-# LANGUAGE Safe #-}

module Haskerwaul.Cone where

import Haskerwaul.Bifunctor
import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Relation.Equivalence
import Haskerwaul.Topos.Elementary

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cone)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Cone_(category_theory))
newtype Cone j c n f = Cone {runCone :: forall x. (Ob j x) => n `c` f x}

coneProperty ::
  (Ob j x, Ob j y, Functor j c f, ElementaryTopos c, Ob c n, EquivalenceRelation c (f y)) =>
  Cone j c n f ->
  x `j` y ->
  n `c` Class c
coneProperty cone f =
  equiv . bimap (map f . runCone cone) (runCone cone) . diagonal

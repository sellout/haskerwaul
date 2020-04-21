module Haskerwaul.Law.Symmetry where

import Haskerwaul.Law
import Haskerwaul.Law.Commutative
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Topos.Elementary

-- | This is simply a more specialized `commutativeLaw`.
--
--   [nLab](https://ncatlab.org/nlab/show/symmetric+relation)
symmetry :: forall c a. (ElementaryTopos c, Ob c a)
            => HomogeneousRelation c a -> Law c (Prod c a a) (Class c)
symmetry = commutativeLaw

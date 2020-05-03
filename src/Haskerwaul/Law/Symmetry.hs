module Haskerwaul.Law.Symmetry where

import Haskerwaul.Law
import Haskerwaul.Law.Commutativity
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

-- | This is simply more specialized `commutativity`.
--
--   [nLab](https://ncatlab.org/nlab/show/symmetric+relation)
symmetry :: forall c a. (ElementaryTopos c, Ob c a)
         => BinaryRelation c a a -> Law c (Prod c a a) (Class c)
symmetry = commutativity

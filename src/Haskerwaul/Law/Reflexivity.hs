module Haskerwaul.Law.Reflexivity where

import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/reflexive+relation)
reflexivity :: (ElementaryTopos c, Ob c a)
            => BinaryRelation c a a -> Law c a (Class c)
reflexivity op' = Law (true . (!)) (op' . diagonal)

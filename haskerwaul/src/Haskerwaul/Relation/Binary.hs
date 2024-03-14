module Haskerwaul.Relation.Binary where

import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/relation#binary_relations)
type BinaryRelation c a b = Prod c a b `c` Class c

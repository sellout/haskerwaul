module Haskerwaul.Relation.Unary where

import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/relation)
type UnaryRelation c a = a `c` Class c

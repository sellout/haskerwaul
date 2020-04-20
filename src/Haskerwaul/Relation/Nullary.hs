module Haskerwaul.Relation.Nullary where

import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/relation)
type NullaryRelation c a = TerminalObject c `c` Class c

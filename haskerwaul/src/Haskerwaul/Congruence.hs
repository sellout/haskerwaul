{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Congruence
  ( module Haskerwaul.Congruence,

    -- * extended modules
    module Haskerwaul.Relation.Equivalence,
  )
where

import Haskerwaul.Category.Complete.Finitely
import Haskerwaul.Relation.Equivalence

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/congruence)
class (FinitelyCompleteCategory c, EquivalenceRelation c a) => Congruence c a

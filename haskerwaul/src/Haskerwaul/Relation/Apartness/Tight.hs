{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Apartness.Tight
  ( module Haskerwaul.Relation.Apartness.Tight,

    -- * extended modules
    module Haskerwaul.Relation.Apartness,
    module Haskerwaul.Relation.Inequality.Tight,
  )
where

import Haskerwaul.Relation.Apartness
import Haskerwaul.Relation.Inequality.Tight

-- |
--
-- = laws
--   [comparison]: @x # z ==> x # y \/ y # z@
--   [connected]:
--     @`Haskerwaul.Relation.Equality.eq` x y \/ `Haskerwaul.Relation.Equality.Decidable.neq` x y@
--   [irreflexive]: @x # x == false@
--   [symmetric]: @x # y ==> y # x@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/apartness+relation)
--
--  __NB__: Instances for this are automatically coalesced.
class
  (TightInequalityRelation c a, ApartnessRelation c a) =>
  TightApartnessRelation c a

instance
  (TightInequalityRelation c a, ApartnessRelation c a) =>
  TightApartnessRelation c a

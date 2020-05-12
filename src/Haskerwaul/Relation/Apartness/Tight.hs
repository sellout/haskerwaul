{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Apartness.Tight
  ( module Haskerwaul.Relation.Apartness.Tight
  -- * extended modules
  , module Haskerwaul.Relation.Apartness
  , module Haskerwaul.Relation.Inequality.Tight
  ) where

import Haskerwaul.Relation.Apartness
import Haskerwaul.Relation.Inequality.Tight

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation)
--
--  __NB__: Instances for this are automatically coalesced.
--
-- = laws
--   [comparison]: @x `#` z ==> x `#` y \/ y `#` z@
--   [connected]: @`eq` x y \/ `neq` x y@
--   [irreflexive]: @x `#` x == false@
--   [symmetric]: @x `#` y ==> y `#` x@
class (TightInequalityRelation c a, ApartnessRelation c a) =>
      TightApartnessRelation c a

instance (TightInequalityRelation c a, ApartnessRelation c a) =>
         TightApartnessRelation c a

{-# language TypeApplications #-}

module Haskerwaul.Law.Distributive.Self.Left where

import Haskerwaul.Law.Distributive.Left
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/self-distributive+operation)
leftSelfDistributiveLaw
  :: forall c a. (CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Law c EqualityRelation (Prod c a (Prod c a a)) a
leftSelfDistributiveLaw op = leftDistributiveLaw op op

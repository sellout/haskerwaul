module Haskerwaul.Law.Distributive.Self.Right where

import Haskerwaul.Law.Distributive.Right
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/self-distributive+operation)
rightSelfDistributiveLaw
  :: forall c a. (CartesianMonoidalCategory c, Ob c a)
  => Prod c a a `c` a -> Law c EqualityRelation (Prod c (Prod c a a) a) a
rightSelfDistributiveLaw op' = rightDistributiveLaw op' op'

{-# language UndecidableInstances #-}

module Haskerwaul.Negation where

import Haskerwaul.Bifunctor
import Haskerwaul.Object
import Haskerwaul.Order.Partial
import Haskerwaul.Relation.Apartness.Tight
import Haskerwaul.Relation.Equivalence
import Haskerwaul.Topos.Elementary

-- | A newtype to indicate the complement of a particular relation.
--
--   [nLab](https://ncatlab.org/nlab/show/negation)
newtype Negate a = Negate { negation :: a }

instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a, Ob c (Negate a)) =>
         HomogeneousRelation c (Negate a) where
  rel = rel . bimap negation negation

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => Preorder c (Negate a)

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => PartialEquivalenceRelation c (Negate a)

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => ToleranceRelation c (Negate a)

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, TightApartnessRelation c a) => PartialOrder c (Negate a)

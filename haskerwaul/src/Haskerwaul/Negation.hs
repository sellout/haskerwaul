{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Negation where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Object
import Haskerwaul.Order.Partial
import Haskerwaul.Relation.Apartness.Tight
import Haskerwaul.Relation.Equivalence
import Haskerwaul.Topos.Elementary

-- | A newtype to indicate the complement of a particular relation.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/negation)
newtype Negate a = Negate {negation :: a}

instance
  (c ~ (->), ElementaryTopos c, ApartnessRelation c a, Ob c (Negate a)) =>
  HomogeneousRelation' c (Negate a)
  where
  rel = ne . bimap negation negation

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => Preorder c (Negate a)

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => PartialEquivalenceRelation c (Negate a)

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, ApartnessRelation c a) => ToleranceRelation c (Negate a)

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/apartness+relation#related_notions)
instance (c ~ (->), ElementaryTopos c, TightApartnessRelation c a) => PartialOrder c (Negate a)

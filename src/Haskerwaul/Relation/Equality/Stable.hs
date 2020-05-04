{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equality.Stable
  ( module Haskerwaul.Relation.Equality.Stable
  -- * extended modules
  , module Haskerwaul.Relation.Equality
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Negation
import Haskerwaul.Object
import Haskerwaul.Relation.Equality
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/decidable+equality)
class EqualityRelation c a => StableEquality c a

instance StableEquality (->) (Negate ())

instance StableEquality (->) (Negate Bool)

instance StableEquality (->) (Negate Natural)

instance StableEquality (->) (Negate Int)

instance StableEquality (->) (Negate Int8)

instance StableEquality (->) (Negate Int16)

instance StableEquality (->) (Negate Int32)

instance StableEquality (->) (Negate Int64)

instance StableEquality (->) (Negate Integer)

instance StableEquality (->) (Negate Word)

instance StableEquality (->) (Negate Word8)

instance StableEquality (->) (Negate Word16)

instance StableEquality (->) (Negate Word32)

instance StableEquality (->) (Negate Word64)

instance StableEquality (->) (Negate Float)

instance StableEquality (->) (Negate Double)

instance (c ~ (->), ElementaryTopos c, StableEquality c a, Ob c (Additive a)) =>
         StableEquality c (Additive a)

instance (c ~ (->), ElementaryTopos c, StableEquality c a, Ob c (Multiplicative a)) =>
         StableEquality c (Multiplicative a)

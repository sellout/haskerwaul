{-# language UndecidableSuperClasses #-}

module Haskerwaul.Relation.Dependency
  ( module Haskerwaul.Relation.Dependency
  -- * extended modules
  , module Haskerwaul.Relation.Tolerance
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Prelude (Double, Float)

import Haskerwaul.Negation
import Haskerwaul.Relation.Tolerance

-- | [Wikipedia](https://en.wikipedia.org/wiki/Dependency_relation)
--
-- = laws
--   [finite]: ?
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`rel` x x = `Haskerwaul.Topos.Elementary.true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`rel` x y ==> `rel` y x@
class ToleranceRelation c a => DependencyRelation c a

instance DependencyRelation (->) (Negate ())

instance DependencyRelation (->) (Negate Bool)

instance DependencyRelation (->) (Negate Int)

instance DependencyRelation (->) (Negate Int8)

instance DependencyRelation (->) (Negate Int16)

instance DependencyRelation (->) (Negate Int32)

instance DependencyRelation (->) (Negate Int64)

instance DependencyRelation (->) (Negate Word)

instance DependencyRelation (->) (Negate Word8)

instance DependencyRelation (->) (Negate Word16)

instance DependencyRelation (->) (Negate Word32)

instance DependencyRelation (->) (Negate Word64)

instance DependencyRelation (->) (Negate Float)

instance DependencyRelation (->) (Negate Double)

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

import Haskerwaul.Relation.Tolerance

-- | [Wikipedia](https://en.wikipedia.org/wiki/Dependency_relation)
--
-- = laws
--   [finite]: ?
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`rel` x x = `Haskerwaul.Topos.Elementary.true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`rel` x y ==> `rel` y x@
class ToleranceRelation c a => DependencyRelation c a

instance DependencyRelation (->) ()

instance DependencyRelation (->) Bool

instance DependencyRelation (->) Int

instance DependencyRelation (->) Int8

instance DependencyRelation (->) Int16

instance DependencyRelation (->) Int32

instance DependencyRelation (->) Int64

instance DependencyRelation (->) Word

instance DependencyRelation (->) Word8

instance DependencyRelation (->) Word16

instance DependencyRelation (->) Word32

instance DependencyRelation (->) Word64

instance DependencyRelation (->) Float

instance DependencyRelation (->) Double

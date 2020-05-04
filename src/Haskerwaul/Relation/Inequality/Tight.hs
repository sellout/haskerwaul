{-# language UndecidableSuperClasses #-}

module Haskerwaul.Relation.Inequality.Tight
  ( module Haskerwaul.Relation.Inequality.Tight
  -- * extended modules
  , module Haskerwaul.Relation.Inequality
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Inequality

-- | [nLab](https://ncatlab.org/nlab/show/inequality+relation)
--
-- = laws
--   [connected]: @`eq` x y \/ `neq` x y@
--   [irreflexive]: @x `neq` x == false@
--   [symmetric]: @x `neq` y ==> y `neq` x@
class InequalityRelation c a => TightInequalityRelation c a

instance TightInequalityRelation (->) ()

instance TightInequalityRelation (->) Bool

instance TightInequalityRelation (->) Natural

instance TightInequalityRelation (->) Int

instance TightInequalityRelation (->) Int8

instance TightInequalityRelation (->) Int16

instance TightInequalityRelation (->) Int32

instance TightInequalityRelation (->) Int64

instance TightInequalityRelation (->) Integer

instance TightInequalityRelation (->) Word

instance TightInequalityRelation (->) Word8

instance TightInequalityRelation (->) Word16

instance TightInequalityRelation (->) Word32

instance TightInequalityRelation (->) Word64

instance TightInequalityRelation (->) Float

instance TightInequalityRelation (->) Double

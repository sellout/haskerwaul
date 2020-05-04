{-# language UndecidableSuperClasses #-}

module Haskerwaul.Relation.Inequality
  ( module Haskerwaul.Relation.Inequality
  -- * extended modules
  , module Haskerwaul.Relation.Homogeneous
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Homogeneous

-- | [nLab](https://ncatlab.org/nlab/show/inequality+relation)
--
-- = laws
--   [irreflexive]: @x `/=` x == false@
--   [symmetric]: @x `/=` y ==> y `/=` x@
class HomogeneousRelation c a => InequalityRelation c a

instance InequalityRelation (->) ()

instance InequalityRelation (->) Bool

instance InequalityRelation (->) Natural

instance InequalityRelation (->) Int

instance InequalityRelation (->) Int8

instance InequalityRelation (->) Int16

instance InequalityRelation (->) Int32

instance InequalityRelation (->) Int64

instance InequalityRelation (->) Integer

instance InequalityRelation (->) Word

instance InequalityRelation (->) Word8

instance InequalityRelation (->) Word16

instance InequalityRelation (->) Word32

instance InequalityRelation (->) Word64

instance InequalityRelation (->) Float

instance InequalityRelation (->) Double

{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Apartness
  ( module Haskerwaul.Relation.Apartness,

    -- * extended modules
    module Haskerwaul.Relation.Inequality,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Relation.Inequality
import Numeric.Natural (Natural)
import Prelude (Double, Float, Integer)

-- | [nLab](https://ncatlab.org/nlab/show/apartness+relation)
--
-- = laws
--   [comparison]: @x # z ==> x # y \/ y # z@
--   [irreflexive]: @x # x == false@
--   [symmetric]: @x # y ==> y # x@
class (InequalityRelation c a) => ApartnessRelation c a

instance ApartnessRelation (->) ()

instance ApartnessRelation (->) Void

instance ApartnessRelation (->) Bool

instance ApartnessRelation (->) Natural

instance ApartnessRelation (->) Int

instance ApartnessRelation (->) Int8

instance ApartnessRelation (->) Int16

instance ApartnessRelation (->) Int32

instance ApartnessRelation (->) Int64

instance ApartnessRelation (->) Integer

instance ApartnessRelation (->) Word

instance ApartnessRelation (->) Word8

instance ApartnessRelation (->) Word16

instance ApartnessRelation (->) Word32

instance ApartnessRelation (->) Word64

instance ApartnessRelation (->) Float

instance ApartnessRelation (->) Double

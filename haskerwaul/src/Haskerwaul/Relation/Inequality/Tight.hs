{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Inequality.Tight
  ( module Haskerwaul.Relation.Inequality.Tight,

    -- * extended modules
    module Haskerwaul.Relation.Inequality,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Relation.Inequality
import Numeric.Natural (Natural)
import Prelude (Double, Float, Integer)

-- |
--
-- = laws
--
--   [connected]: @`Haskerwaul.Relation.Equality.eq` x y \/ `Haskerwaul.Relation.Equality.Decidable.neq` x y@
--   [irreflexive]: @`Haskerwaul.Relation.Equality.Decidable.neq` x x == false@
--   [symmetric]: @`Haskerwaul.Relation.Equality.Decidable.neq` x y ==> `Haskerwaul.Relation.Equality.Decidable.neq` y x@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/inequality+relation)
class (InequalityRelation c a) => TightInequalityRelation c a

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

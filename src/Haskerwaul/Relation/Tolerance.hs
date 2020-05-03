{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Tolerance
  ( module Haskerwaul.Relation.Tolerance
  -- * extended modules
  , module Haskerwaul.Relation.Homogeneous
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [Wikipedia](https://en.wikipedia.org/wiki/Tolerance_relation)
--
-- = laws
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`rel` x x = `true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`rel` x y ==> `rel` y x@
class HomogeneousRelation c a => ToleranceRelation c a

instance ToleranceRelation (->) ()

instance ToleranceRelation (->) Bool

instance ToleranceRelation (->) Natural

instance ToleranceRelation (->) Int

instance ToleranceRelation (->) Int8

instance ToleranceRelation (->) Int16

instance ToleranceRelation (->) Int32

instance ToleranceRelation (->) Int64

instance ToleranceRelation (->) Integer

instance ToleranceRelation (->) Word

instance ToleranceRelation (->) Word8

instance ToleranceRelation (->) Word16

instance ToleranceRelation (->) Word32

instance ToleranceRelation (->) Word64

instance ToleranceRelation (->) Float

instance ToleranceRelation (->) Double

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Meet a)) =>
         ToleranceRelation c (Meet a)

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Join a)) =>
         ToleranceRelation c (Join a)

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Additive a)) =>
         ToleranceRelation c (Additive a)

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Multiplicative a)) =>
         ToleranceRelation c (Multiplicative a)

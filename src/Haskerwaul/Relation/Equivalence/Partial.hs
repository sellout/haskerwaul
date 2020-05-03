{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence.Partial
  ( module Haskerwaul.Relation.Equivalence.Partial
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
import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- |
-- = laws
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`equiv` x y ==> `equiv` y x@
--   [transitivity]: @`equiv` x y && `equiv` y z ==> `equiv` x z@
class HomogeneousRelation c a => PartialEquivalenceRelation c a

equiv :: PartialEquivalenceRelation c a => BinaryRelation c a a
equiv = rel

instance PartialEquivalenceRelation (->) ()

instance PartialEquivalenceRelation (->) Bool

instance PartialEquivalenceRelation (->) Natural

instance PartialEquivalenceRelation (->) Int

instance PartialEquivalenceRelation (->) Int8

instance PartialEquivalenceRelation (->) Int16

instance PartialEquivalenceRelation (->) Int32

instance PartialEquivalenceRelation (->) Int64

instance PartialEquivalenceRelation (->) Integer

instance PartialEquivalenceRelation (->) Word

instance PartialEquivalenceRelation (->) Word8

instance PartialEquivalenceRelation (->) Word16

instance PartialEquivalenceRelation (->) Word32

instance PartialEquivalenceRelation (->) Word64

instance PartialEquivalenceRelation (->) Float

instance PartialEquivalenceRelation (->) Double

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Meet a)) =>
         PartialEquivalenceRelation c (Meet a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Join a)) =>
         PartialEquivalenceRelation c (Join a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Additive a)) =>
         PartialEquivalenceRelation c (Additive a)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Multiplicative a)) =>
         PartialEquivalenceRelation c (Multiplicative a)

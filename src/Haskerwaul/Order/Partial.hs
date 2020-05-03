{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Order.Partial
  ( module Haskerwaul.Order.Partial
  -- * extended modules
  , module Haskerwaul.Preorder
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Lattice
import Haskerwaul.Preorder
import Haskerwaul.Order.Canonical
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/partial+order)
--
-- = laws
--   [antisymmetry]: @`le` (x, y) && `le (y, z)` ==> `Haskerwaul.Relation.Equivalence.Partial.equiv` (x, y)@
class Preorder c a => PartialOrder c a

-- | We can get an equivalence relation from any partial order, but we may also
--   define a partial order in terms of an equivalence relation, so this exists
--   to be used as a default implementation when possible.
defaultEquivalence :: forall c a. (c ~ (->), ElementaryTopos c, PartialOrder c a)
                   => BinaryRelation c a a
defaultEquivalence = meet . bimap (le @c) (le @c . braid) . diagonal

instance PartialOrder (->) ()

instance PartialOrder (->) Bool

instance PartialOrder (->) Natural

instance PartialOrder (->) Int

instance PartialOrder (->) Int8

instance PartialOrder (->) Int16

instance PartialOrder (->) Int32

instance PartialOrder (->) Int64

instance PartialOrder (->) Integer

instance PartialOrder (->) Word

instance PartialOrder (->) Word8

instance PartialOrder (->) Word16

instance PartialOrder (->) Word32

instance PartialOrder (->) Word64

instance PartialOrder (->) Float

instance PartialOrder (->) Double

instance PartialOrder (->) (Canonical Bool)

instance PartialOrder (->) (Canonical Natural)

instance PartialOrder (->) (Canonical Int)

instance PartialOrder (->) (Canonical Int8)

instance PartialOrder (->) (Canonical Int16)

instance PartialOrder (->) (Canonical Int32)

instance PartialOrder (->) (Canonical Int64)

instance PartialOrder (->) (Canonical Integer)

instance PartialOrder (->) (Canonical Word)

instance PartialOrder (->) (Canonical Word8)

instance PartialOrder (->) (Canonical Word16)

instance PartialOrder (->) (Canonical Word32)

instance PartialOrder (->) (Canonical Word64)

instance PartialOrder (->) (Canonical Float)

instance PartialOrder (->) (Canonical Double)

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Preorder
  ( module Haskerwaul.Preorder
  -- * extended modules
  , module Haskerwaul.Relation.Homogeneous
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Object
import Haskerwaul.Order.Canonical
import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | There are too many valid instances of this for almost any type for it to be
--   something to implement directly. This is generally provided automatically
--   via other instances where there is a /canonical/ instance for this type.
--
--  [nLab](https://ncatlab.org/nlab/show/preorder)
--
-- = laws
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`le` (x, x) == `true` ()@
--   [transitivity]: @`le` (x, y) && `le (y, z)` ==> `le` (x, z)@
class HomogeneousRelation c a => Preorder c a

le :: Preorder c a => BinaryRelation c a a
le = rel

instance Preorder (->) (Canonical Bool)

instance Preorder (->) (Canonical Natural)

instance Preorder (->) (Canonical Int)

instance Preorder (->) (Canonical Int8)

instance Preorder (->) (Canonical Int16)

instance Preorder (->) (Canonical Int32)

instance Preorder (->) (Canonical Int64)

instance Preorder (->) (Canonical Integer)

instance Preorder (->) (Canonical Word)

instance Preorder (->) (Canonical Word8)

instance Preorder (->) (Canonical Word16)

instance Preorder (->) (Canonical Word32)

instance Preorder (->) (Canonical Word64)

instance Preorder (->) (Canonical Float)

instance Preorder (->) (Canonical Double)

instance (c ~ (->), ElementaryTopos c, Preorder c a, Ob c (Additive a)) =>
         Preorder c (Additive a)

instance (c ~ (->), ElementaryTopos c, Preorder c a, Ob c (Multiplicative a)) =>
         Preorder c (Multiplicative a)

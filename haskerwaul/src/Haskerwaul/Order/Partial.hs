{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Order.Partial
  ( module Haskerwaul.Order.Partial,

    -- * extended modules
    module Haskerwaul.Preorder,
  )
where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import qualified GHC.Real as Base
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice
import Haskerwaul.Object
import Haskerwaul.Order.Canonical
import Haskerwaul.Preorder
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary
import Numeric.Natural (Natural)
import Prelude (Double, Float, Integer)

-- | [nLab](https://ncatlab.org/nlab/show/partial+order)
--
-- = laws
--   [antisymmetry]: @`le` (x, y) && `le (y, z)` ==> `Haskerwaul.Relation.Equivalence.Partial.equiv` (x, y)@
class (Preorder c a) => PartialOrder c a

-- | We can get an equivalence relation from any partial order, but we may also
--   define a partial order in terms of an equivalence relation, so this exists
--   to be used as a default implementation when possible.
defaultEquivalence ::
  forall c a.
  (c ~ (->), ElementaryTopos c, PartialOrder c a) =>
  BinaryRelation c a a
defaultEquivalence = meet . bimap (le @c) (le @c . to braid) . diagonal

instance PartialOrder (->) ()

instance PartialOrder (->) Void

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

instance (Base.Integral a) => PartialOrder (->) (Canonical (Ratio a))

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialOrder c a, Ob c (Additive a)) =>
  PartialOrder c (Additive a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, PartialOrder c a, Ob c (Multiplicative a)) =>
  PartialOrder c (Multiplicative a)

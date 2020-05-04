{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Homogeneous
  ( module Haskerwaul.Relation.Homogeneous
  -- * extended modules
  , module Haskerwaul.Relation.Binary
  ) where

import           Data.Bool (Bool (..))
import qualified Data.Eq as Base
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import qualified Data.Ord as Base
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Order.Canonical
import Haskerwaul.Relation.Binary
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/relation)
--
--  __NB__: There are like a billion of these for any type. This is a type class
--          to give a common super-class for orders and other relations that are
--          at least a _bit_ more constrained. However, since most types have an
--         `Haskerwaul.Relation.Equality.EqualityRelation` instance, it means
--          that everything else is going to be newtyped. It doesn't seem ideal.
class Ob c a => HomogeneousRelation c a where
  rel :: BinaryRelation c a a

instance HomogeneousRelation (->) () where
  rel ((), ()) = False

instance HomogeneousRelation (->) Bool where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Natural where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Int where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Int8 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Int16 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Int32 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Int64 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Integer where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Word where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Word8 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Word16 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Word32 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Word64 where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Float where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) Double where
  rel = from curry (Base./=)

instance HomogeneousRelation (->) (Canonical Bool) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Natural) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Int) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Int8) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Int16) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Int32) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Int64) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Integer) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Word) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Word8) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Word16) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Word32) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Word64) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Float) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation (->) (Canonical Double) where
  rel = from curry (Base.<=) . bimap decanonicalize decanonicalize

instance (c ~ (->), ElementaryTopos c, HomogeneousRelation c a, Ob c (Meet a)) =>
         HomogeneousRelation c (Meet a) where
  rel = rel . bimap getMeet getMeet

instance (c ~ (->), ElementaryTopos c, HomogeneousRelation c a, Ob c (Join a)) =>
         HomogeneousRelation c (Join a) where
  rel = rel . bimap getJoin getJoin

instance (c ~ (->), ElementaryTopos c, HomogeneousRelation c a, Ob c (Additive a)) =>
         HomogeneousRelation c (Additive a) where
  rel = rel . bimap sum sum

instance (c ~ (->), ElementaryTopos c, HomogeneousRelation c a, Ob c (Multiplicative a)) =>
         HomogeneousRelation c (Multiplicative a) where
  rel = rel . bimap product product

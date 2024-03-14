{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Homogeneous
  ( module Haskerwaul.Relation.Homogeneous
  -- * extended modules
  , module Haskerwaul.Relation.Binary
  ) where

import           Data.Bool (Bool (..))
import qualified Data.Eq as Base
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import qualified Data.Ord as Base
import           Data.Ratio (Ratio)
import           Data.Void (Void)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified GHC.Real as Base
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Order.Canonical
import Haskerwaul.Relation.Binary
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/relation)
type HomogeneousRelation c a = BinaryRelation c a a

-- | This is not a real thing, but exists to facilitate the interconnectedness
--   of the subclasses, much like `Magma` (which /is/ real) does on the
--   algebraic side.
--
--   Each instance has at least two of the following three properties:
-- - commutative
-- - reflexive
-- - transitive
class Ob c a => HomogeneousRelation' c a where
  rel :: HomogeneousRelation c a

instance HomogeneousRelation' (->) () where
  rel ((), ()) = True

instance HomogeneousRelation' (->) Void where
  rel = uncurry $ \case

instance HomogeneousRelation' (->) Bool where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Natural where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Int where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Int8 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Int16 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Int32 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Int64 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Integer where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Word where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Word8 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Word16 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Word32 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Word64 where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Float where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) Double where
  rel = uncurry (Base.==)

instance HomogeneousRelation' (->) (Canonical Bool) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Natural) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Int) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Int8) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Int16) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Int32) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Int64) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Integer) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Word) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Word8) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Word16) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Word32) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Word64) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Float) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance HomogeneousRelation' (->) (Canonical Double) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance Base.Integral a => HomogeneousRelation' (->) (Canonical (Ratio a)) where
  rel = uncurry (Base.<=) . bimap decanonicalize decanonicalize

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Const a b)) =>
         HomogeneousRelation' c (Const a b) where
  rel = rel . bimap getConst getConst

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Identity a)) =>
         HomogeneousRelation' c (Identity a) where
  rel = rel . bimap runIdentity runIdentity

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Meet a)) =>
         HomogeneousRelation' c (Meet a) where
  rel = rel . bimap getMeet getMeet

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Join a)) =>
         HomogeneousRelation' c (Join a) where
  rel = rel . bimap getJoin getJoin

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Additive a)) =>
         HomogeneousRelation' c (Additive a) where
  rel = rel . bimap sum sum

instance {-# incoherent #-}
         (c ~ (->), ElementaryTopos c, HomogeneousRelation' c a, Ob c (Multiplicative a)) =>
         HomogeneousRelation' c (Multiplicative a) where
  rel = rel . bimap product product

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence.Partial where

import qualified Data.Eq as Base
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- |
-- = laws
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`equiv` x y ==> `equiv` y x@
--   [transitivity]: @`equiv` x y && `equiv` y z ==> `equiv` x z@
class Ob c a => PartialEquivalenceRelation c a where
  equiv :: HomogeneousRelation c a

instance PartialEquivalenceRelation (->) Natural where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Int where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Int8 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Int16 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Int32 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Int64 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Integer where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Word where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Word8 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Word16 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Word32 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Word64 where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Float where
  equiv = from curry (Base.==)

instance PartialEquivalenceRelation (->) Double where
  equiv = from curry (Base.==)

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Meet a)) =>
         PartialEquivalenceRelation c (Meet a) where
  equiv = equiv . bimap getMeet getMeet

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Join a)) =>
         PartialEquivalenceRelation c (Join a) where
  equiv = equiv . bimap getJoin getJoin

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Additive a)) =>
         PartialEquivalenceRelation c (Additive a) where
  equiv = equiv . bimap sum sum

instance (c ~ (->), ElementaryTopos c, PartialEquivalenceRelation c a, Ob c (Multiplicative a)) =>
         PartialEquivalenceRelation c (Multiplicative a) where
  equiv = equiv . bimap product product

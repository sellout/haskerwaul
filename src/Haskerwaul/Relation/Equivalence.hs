{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equivalence where

import qualified Data.Eq as Base
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Semiring.Components
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Topos.Elementary

class Ob c a => EquivalenceRelation c a where
  equiv :: HomogeneousRelation c a

instance EquivalenceRelation (->) Natural where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Int where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Int8 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Int16 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Int32 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Int64 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Integer where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Word where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Word8 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Word16 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Word32 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Word64 where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Float where
  equiv = from curry (Base.==)

instance EquivalenceRelation (->) Double where
  equiv = from curry (Base.==)

instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Ob c (Meet a)) =>
         EquivalenceRelation c (Meet a) where
  equiv = equiv . bimap getMeet getMeet

instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Ob c (Join a)) =>
         EquivalenceRelation c (Join a) where
  equiv = equiv . bimap getJoin getJoin

instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Ob c (Additive a)) =>
         EquivalenceRelation c (Additive a) where
  equiv = equiv . bimap sum sum

instance (c ~ (->), ElementaryTopos c, EquivalenceRelation c a, Ob c (Multiplicative a)) =>
         EquivalenceRelation c (Multiplicative a) where
  equiv = equiv . bimap product product

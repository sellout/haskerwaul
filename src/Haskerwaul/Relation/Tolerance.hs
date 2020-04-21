{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Tolerance where

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

-- | [Wikipedia](https://en.wikipedia.org/wiki/Tolerance_relation)
--
-- = laws
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`tolerate` x x = `true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`tolerate` x y ==> `tolerate` y x@
class Ob c a => ToleranceRelation c a where
  tolerate :: HomogeneousRelation c a

instance ToleranceRelation (->) Natural where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Int where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Int8 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Int16 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Int32 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Int64 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Integer where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Word where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Word8 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Word16 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Word32 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Word64 where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Float where
  tolerate = from curry (Base.==)

instance ToleranceRelation (->) Double where
  tolerate = from curry (Base.==)

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Meet a)) =>
         ToleranceRelation c (Meet a) where
  tolerate = tolerate . bimap getMeet getMeet

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Join a)) =>
         ToleranceRelation c (Join a) where
  tolerate = tolerate . bimap getJoin getJoin

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Additive a)) =>
         ToleranceRelation c (Additive a) where
  tolerate = tolerate . bimap sum sum

instance (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Multiplicative a)) =>
         ToleranceRelation c (Multiplicative a) where
  tolerate = tolerate . bimap product product

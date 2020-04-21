{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Preorder where

import           Data.Int (Int, Int8, Int16, Int32, Int64)
import qualified Data.Ord as Base
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
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
class Ob c a => Preorder c a where
  le :: HomogeneousRelation c a

instance Preorder (->) Natural where
  le = from curry (Base.<=)

instance Preorder (->) Int where
  le = from curry (Base.<=)

instance Preorder (->) Int8 where
  le = from curry (Base.<=)

instance Preorder (->) Int16 where
  le = from curry (Base.<=)

instance Preorder (->) Int32 where
  le = from curry (Base.<=)

instance Preorder (->) Int64 where
  le = from curry (Base.<=)

instance Preorder (->) Integer where
  le = from curry (Base.<=)

instance Preorder (->) Word where
  le = from curry (Base.<=)

instance Preorder (->) Word8 where
  le = from curry (Base.<=)

instance Preorder (->) Word16 where
  le = from curry (Base.<=)

instance Preorder (->) Word32 where
  le = from curry (Base.<=)

instance Preorder (->) Word64 where
  le = from curry (Base.<=)

instance Preorder (->) Float where
  le = from curry (Base.<=)

instance Preorder (->) Double where
  le = from curry (Base.<=)

instance (c ~ (->), ElementaryTopos c, Preorder c a, Ob c (Additive a)) =>
         Preorder c (Additive a) where
  le = le . bimap sum sum

instance (c ~ (->), ElementaryTopos c, Preorder c a, Ob c (Multiplicative a)) =>
         Preorder c (Multiplicative a) where
  le = le . bimap product product

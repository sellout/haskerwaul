{-# language UndecidableSuperClasses #-}

module Haskerwaul.Order.Total
  ( module Haskerwaul.Order.Total
  -- * extended modules
  , module Haskerwaul.Order.Prefix
  ) where

import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Proxy (Proxy(..))
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float, Integer)

import Haskerwaul.Algebra.Boolean
import Haskerwaul.Order.Prefix
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/total+order)
--
-- = laws
--   [totality]: @`le` (x, y) || `le` (y, x) == `true` ()@
class PrefixOrder c a => TotalOrder c a

ge :: (ElementaryTopos c, TotalOrder c a) => HomogeneousRelation c a
ge = le . braid

gt :: forall c a. (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), TotalOrder c a)
   => HomogeneousRelation c a
gt = complement (Proxy :: Proxy (Prod c)) . le

lt :: (ElementaryTopos c, BooleanAlgebra c (Prod c) (Class c), TotalOrder c a)
   => HomogeneousRelation c a
lt = gt . braid

instance TotalOrder (->) Natural

instance TotalOrder (->) Int

instance TotalOrder (->) Int8

instance TotalOrder (->) Int16

instance TotalOrder (->) Int32

instance TotalOrder (->) Int64

instance TotalOrder (->) Integer

instance TotalOrder (->) Word

instance TotalOrder (->) Word8

instance TotalOrder (->) Word16

instance TotalOrder (->) Word32

instance TotalOrder (->) Word64

instance TotalOrder (->) Float

instance TotalOrder (->) Double

{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Relation.Equality.Decidable
  ( module Haskerwaul.Relation.Equality.Decidable
  -- * extended modules
  , module Haskerwaul.Relation.Equality.Stable
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Proxy (Proxy (..))
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Double, Float)

import Haskerwaul.Lattice.Orthocomplemented
import Haskerwaul.Negation
import Haskerwaul.Object
import Haskerwaul.Relation.Equality.Stable
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/decidable+equality)
class StableEquality c a => DecidableEquality c a

neq
  :: forall c a
   . ( ElementaryTopos c
     , OrthocomplementedLattice c (Prod c) (Class c)
     , DecidableEquality c a)
  => BinaryRelation c a a
neq = complement (Proxy :: Proxy (Prod c)) . eq

instance DecidableEquality (->) (Negate ())

instance DecidableEquality (->) (Negate Bool)

instance DecidableEquality (->) (Negate Natural)

instance DecidableEquality (->) (Negate Int)

instance DecidableEquality (->) (Negate Int8)

instance DecidableEquality (->) (Negate Int16)

instance DecidableEquality (->) (Negate Int32)

instance DecidableEquality (->) (Negate Int64)

instance DecidableEquality (->) (Negate Word)

instance DecidableEquality (->) (Negate Word8)

instance DecidableEquality (->) (Negate Word16)

instance DecidableEquality (->) (Negate Word32)

instance DecidableEquality (->) (Negate Word64)

instance DecidableEquality (->) (Negate Float)

instance DecidableEquality (->) (Negate Double)

instance (c ~ (->), ElementaryTopos c, DecidableEquality c a, Ob c (Additive a)) =>
         DecidableEquality c (Additive a)

instance (c ~ (->), ElementaryTopos c, DecidableEquality c a, Ob c (Multiplicative a)) =>
         DecidableEquality c (Multiplicative a)

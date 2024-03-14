{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.GaloisConnection where

import Data.Proxy (Proxy (..))
import Data.Ratio (Rational)
import GHC.Float (Double, Float)
import qualified GHC.Float as Base
import qualified GHC.Real as Base
import Haskerwaul.Order.Canonical
import Haskerwaul.Preorder
import Haskerwaul.Semigroupoid

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Galois+connection)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Galois_connection)
class (Preorder c a, Preorder c b) => GaloisConnection c a b where
  lowerAdjoint :: a `c` b
  upperAdjoint :: b `c` a

associatedClosure :: forall c a b. (Semigroupoid c, GaloisConnection c a b) => Proxy b -> a `c` a
associatedClosure Proxy = upperAdjoint @_ @_ @b . lowerAdjoint

associatedKernel :: forall c a b. (Semigroupoid c, GaloisConnection c a b) => Proxy a -> b `c` b
associatedKernel Proxy = lowerAdjoint @_ @a . upperAdjoint

instance GaloisConnection (->) (Canonical Double) (Canonical Float) where
  lowerAdjoint = Canonical . Base.double2Float . decanonicalize
  upperAdjoint = Canonical . Base.float2Double . decanonicalize

instance GaloisConnection (->) (Canonical Double) (Canonical Rational) where
  lowerAdjoint = Canonical . Base.toRational . decanonicalize
  upperAdjoint = Canonical . Base.fromRational . decanonicalize

instance GaloisConnection (->) (Canonical Float) (Canonical Rational) where
  lowerAdjoint = Canonical . Base.toRational . decanonicalize
  upperAdjoint = Canonical . Base.fromRational . decanonicalize

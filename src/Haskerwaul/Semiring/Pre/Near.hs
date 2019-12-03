{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Pre.Near
  ( module Haskerwaul.Semiring.Pre.Near
  -- * extended modules
  , module Haskerwaul.Semigroup
  ) where

import           Prelude ((+), (*), (-), Int, Integer)
import qualified Data.Monoid as Base
import qualified Data.Semigroup as Base
import           Numeric.Natural (Natural)

import Haskerwaul.Bifunctor
import Haskerwaul.Magma.Commutative
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Semigroup
import Haskerwaul.Semigroupoid

newtype Additive a = Add { sum :: a }
newtype Multiplicative a = Multiply { product :: a }

class ( Semigroup c t (Additive a)
      , Semigroup c t (Multiplicative a)) =>
      NearPreSemiring c t a where

instance ( Semigroup c t (Additive a)
         , Semigroup c t (Multiplicative a)) =>
         NearPreSemiring c t a

add :: (c ~ (->), NearPreSemiring c t a, Bifunctor c c c t) => t a a `c` a
add = sum . op . bimap Add Add

multiply :: (c ~ (->), NearPreSemiring c t a, Bifunctor c c c t)
         => t a a `c` a
multiply = product . op . bimap Multiply Multiply

instance CommutativeMagma (->) (,) (Additive Natural)

instance Base.Semigroup (Additive Natural) where
  Add x <> Add y = Add (x + y)

instance Base.Monoid (Additive Natural) where
  mempty = Add 0

instance CommutativeMagma (->) (,) (Multiplicative Natural)

instance Base.Semigroup (Multiplicative Natural) where
  Multiply x <> Multiply y = Multiply (x * y)

instance Base.Monoid (Multiplicative Natural) where
  mempty = Multiply 1

instance CommutativeMagma (->) (,) (Additive Integer)

instance Base.Semigroup (Additive Integer) where
  Add x <> Add y = Add (x + y)

instance Base.Monoid (Additive Integer) where
  mempty = Add 0

instance LeftQuasigroup (->) (,) (Additive Integer) where
  leftQuotient (Add x, Add y) = Add (y - x)

instance RightQuasigroup (->) (,) (Additive Integer) where
  rightQuotient (Add x, Add y) = Add (x - y)

instance CommutativeMagma (->) (,) (Multiplicative Integer)

instance Base.Semigroup (Multiplicative Integer) where
  Multiply x <> Multiply y = Multiply (x * y)

instance Base.Monoid (Multiplicative Integer) where
  mempty = Multiply 1

instance CommutativeMagma (->) (,) (Additive Int)

instance Base.Semigroup (Additive Int) where
  Add x <> Add y = Add (x + y)

instance Base.Monoid (Additive Int) where
  mempty = Add 0

instance LeftQuasigroup (->) (,) (Additive Int) where
  leftQuotient (Add x, Add y) = Add (y - x)

instance RightQuasigroup (->) (,) (Additive Int) where
  rightQuotient (Add x, Add y) = Add (x - y)

instance CommutativeMagma (->) (,) (Multiplicative Int)

instance Base.Semigroup (Multiplicative Int) where
  Multiply x <> Multiply y = Multiply (x * y)

instance Base.Monoid (Multiplicative Int) where
  mempty = Multiply 1

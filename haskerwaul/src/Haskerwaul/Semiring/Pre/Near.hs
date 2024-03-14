{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Pre.Near
  ( module Haskerwaul.Semiring.Pre.Near
  -- * extended modules
  , module Haskerwaul.Semigroup
  , module Haskerwaul.Semiring.Components
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Semigroup
import Haskerwaul.Semiring.Components

class ( Semigroup c t (Additive a)
      , Semigroup c t (Multiplicative a)) =>
      NearPreSemiring c t a where

instance ( Semigroup c t (Additive a)
         , Semigroup c t (Multiplicative a)) =>
         NearPreSemiring c t a

add :: (c ~ (->), SemigroupalCategory c t, NearPreSemiring c t a) => t a a `c` a
add = sum . op . bimap Add Add

multiply :: (c ~ (->), SemigroupalCategory c t, NearPreSemiring c t a)
         => t a a `c` a
multiply = product . op . bimap Multiply Multiply

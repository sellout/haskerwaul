{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Pre.Near
  ( module Haskerwaul.Semiring.Pre.Near,

    -- * extended modules
    module Haskerwaul.Semigroup,
    module Haskerwaul.Semiring.Components,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Semigroup
import Haskerwaul.Semiring.Components

class
  ( Semigroup c t (Additive a),
    Semigroup c t (Multiplicative a)
  ) =>
  NearPreSemiring c t a

instance
  ( Semigroup c t (Additive a),
    Semigroup c t (Multiplicative a)
  ) =>
  NearPreSemiring c t a

add :: (c ~ (->), SemigroupalCategory c t, NearPreSemiring c t a) => t a a `c` a
add = sum . op . bimap Add Add

multiply ::
  (c ~ (->), SemigroupalCategory c t, NearPreSemiring c t a) =>
  t a a `c` a
multiply = product . op . bimap Multiply Multiply

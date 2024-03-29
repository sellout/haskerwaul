{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Hemiring.Near
  ( module Haskerwaul.Hemiring.Near,

    -- * extended modules
    module Haskerwaul.Monoid,
    module Haskerwaul.Semiring.Pre.Near,
  )
where

import Data.Proxy (Proxy)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Category.Monoidal
import Haskerwaul.Monoid
import Haskerwaul.Semiring.Pre.Near

-- | [nLab](https://ncatlab.org/nlab/show/near-ring)
class
  (NearPreSemiring c t a, Monoid c t (Additive a)) =>
  NearHemiring c t a

-- | [nLab](https://ncatlab.org/nlab/show/zero)
zero ::
  (c ~ (->), MonoidalCategory c t, NearHemiring c t a) =>
  Proxy t ->
  Unit c t `c` a
zero t = sum . unit t

instance
  (Monoid c t (Additive a), Semigroup c t (Multiplicative a)) =>
  NearHemiring c t a

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Near
  ( module Haskerwaul.Semiring.Near,

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

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/near-ring)
--
--  __NB__: Instances for this are automatically coalesced.
class
  (NearPreSemiring c t a, Monoid c t (Multiplicative a)) =>
  NearSemiring c t a

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/one)
one ::
  (c ~ (->), MonoidalCategory c t, NearSemiring c t a) =>
  Proxy t ->
  Unit c t `c` a
one t = product . unit t

instance
  (Semigroup c t (Additive a), Monoid c t (Multiplicative a)) =>
  NearSemiring c t a

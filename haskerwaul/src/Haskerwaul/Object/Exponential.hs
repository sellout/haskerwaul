{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Object.Exponential
  ( module Haskerwaul.Object.Exponential,

    -- * extended modules
    module Haskerwaul.Residual,
  )
where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Residual

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/residual)
--
--  __NB__: Instances for this are automatically coalesced
class (CartesianMonoidalCategory c, Residual c (Prod c) x y) => Exponential c x y where
  type Exp c x y :: ok

-- | Implied by this and other instances is that every
--  `CartesianClosedMonoidalCategory` has all `Exponential`s. That doesn't
--   require any additional instances to be defined, however.
instance (CartesianMonoidalCategory c, Residual c (Prod c) x y) => Exponential c x y where
  type Exp c x y = Res c (Prod c) x y

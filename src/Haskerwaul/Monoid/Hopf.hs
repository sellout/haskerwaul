{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Hopf
  ( module Haskerwaul.Monoid.Hopf
  -- * extended modules
  , module Haskerwaul.Bimonoid
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Bimonoid

-- | [nLab](https://ncatlab.org/nlab/show/Hopf+monoid)
class Bimonoid c t a => HopfMonoid c t a where
  antipode :: Proxy t -> a `c` a

-- -- | Every `Group` in a `CartesianMonoidalCategoy` is a `HopfMonoid`.
-- --   [nLab](https://ncatlab.org/nlab/show/bimonoid#examples)
-- instance (CartesianMonoidalCategoy c, Group c (Prod c) a) =>
--          HopfMonoid c (Prod c) a where
--   antipode = inverse

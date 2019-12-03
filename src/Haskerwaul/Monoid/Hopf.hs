{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Hopf
  ( module Haskerwaul.Monoid.Hopf
  -- * extended modules
  , module Haskerwaul.Bimonoid
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Bimonoid

class Bimonoid k t a => HopfMonoid k t a where
  antipode :: Proxy t -> a `k` a

-- -- | Every `Group` in a `CartesianMonoidalCategoy` is a `HopfMonoid`.
-- --   https://ncatlab.org/nlab/show/bimonoid#examples
-- instance (CartesianMonoidalCategoy k, Group k (Prod k) a) =>
--          HopfMonoid k (Prod k) a where
--   antipode = inverse

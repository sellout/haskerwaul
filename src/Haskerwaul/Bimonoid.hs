{-# language UndecidableSuperClasses #-}

module Haskerwaul.Bimonoid
  ( module Haskerwaul.Bimonoid
  -- * extended modules
  , module Haskerwaul.Comonoid
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Comonoid
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/bimonoid)
class (Monoid c t a, Comonoid c t a) => Bimonoid c t a

-- -- | Every `Monoid` in a `CartesianMonoidalCategoy` is a `Bimonoid`.
-- --   https://ncatlab.org/nlab/show/bimonoid#examples
-- instance (CartesianMonoidalCategory c, t ~ Prod c, Monoid c t a) =>
--          Bimonoid c t a

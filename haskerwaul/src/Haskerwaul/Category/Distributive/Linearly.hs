{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Distributive.Linearly
  ( module Haskerwaul.Category.Distributive.Linearly,

    -- * extended modules
    module Haskerwaul.Category.Monoidal,
  )
where

import Haskerwaul.Category.Monoidal
import Haskerwaul.Object

-- | a.k.a. "weakly distributive category"
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/linearly+distributive+category)
class
  (MonoidalCategory c p, MonoidalCategory c et) =>
  LinearlyDistributiveCategory c p et
  where
  deltaL :: (Ob c x, Ob c y, Ob c z) => p x (et y z) `c` et (p x y) z
  deltaR :: (Ob c x, Ob c y, Ob c z) => p (et x y) z `c` et x (p y z)

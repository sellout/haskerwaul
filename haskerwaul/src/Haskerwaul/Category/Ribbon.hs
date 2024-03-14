{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Ribbon
  ( module Haskerwaul.Category.Ribbon,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Balanced,
    module Haskerwaul.Category.Monoidal.Rigid,
  )
where

import Haskerwaul.Category.Monoidal.Balanced
import Haskerwaul.Category.Monoidal.Rigid

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/ribbon+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Ribbon_category)
--
--  __NB__: Instances for this are automatically coalesced.
class (BalancedMonoidalCategory c t, RigidMonoidalCategory c t) => RibbonCategory c t

instance (BalancedMonoidalCategory c t, RigidMonoidalCategory c t) => RibbonCategory c t

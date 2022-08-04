{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.LocallyCartesian
  ( module Haskerwaul.Category.LocallyCartesian,

    -- * extended modules
    module Haskerwaul.Category,
  )
where

import Haskerwaul.Category

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/locally-cartesian+category)
--
--  __NB__: Instances for this are automatically coalesced.
class
  ( Category c
  -- forall a. Ob c a => CartesianMonoidalCategory (SliceCategory c a)
  ) =>
  LocallyCartesianCategory c

instance
  ( Category c
  -- forall a. Ob c a => CartesianMonoidalCategory (SliceCategory c a)
  ) =>
  LocallyCartesianCategory c

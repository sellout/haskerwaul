{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Rack
  ( module Haskerwaul.Rack,

    -- * extended modules
    module Haskerwaul.Shelf,
  )
where

import Haskerwaul.Shelf

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/rack)
class (Shelf c t a) => Rack c t a where
  rightAction :: t a a `c` a

leftAction :: (Rack c t a) => t a a `c` a
leftAction = op

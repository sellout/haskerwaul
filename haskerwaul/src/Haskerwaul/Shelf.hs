{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Shelf
  ( module Haskerwaul.Shelf,

    -- * extended modules
    module Haskerwaul.Shelf.Left,
    module Haskerwaul.Shelf.Right,
  )
where

import Haskerwaul.Shelf.Left
import Haskerwaul.Shelf.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/shelf)
--
--  __NB__: Instances for this are automatically coalesced.
class (LeftShelf c t a, RightShelf c t a) => Shelf c t a

instance (LeftShelf c t a, RightShelf c t a) => Shelf c t a

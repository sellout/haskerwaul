{-# language UndecidableSuperClasses #-}

module Haskerwaul.Shelf.Left
  ( module Haskerwaul.Shelf.Left
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Magma

-- |
-- = laws
--
--   [left self-distributive]: @x `op` (y `op` z) == (x `op` y) `op` (x `op` z)@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/shelf)
class Magma c t a => LeftShelf c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Shelf.Right
  ( module Haskerwaul.Shelf.Right
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Magma

-- |
-- = laws
--
--   [right self-distributive]: @(x `op` y) `op` z == (x `op` z) `op` (y `op` z)@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/shelf)
class Magma c t a => RightShelf c t a

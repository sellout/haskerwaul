{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Invertible.Right
  ( module Haskerwaul.Magma.Invertible.Right
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Magma

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/invertible+magma)
class Magma c t a => RightInvertibleMagma c t a

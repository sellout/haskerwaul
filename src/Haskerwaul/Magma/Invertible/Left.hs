{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Invertible.Left
  ( module Haskerwaul.Magma.Invertible.Left
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Magma

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/invertible+magma)
class Magma c t a => LeftInvertibleMagma c t a

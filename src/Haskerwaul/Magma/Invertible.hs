{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Invertible
  ( module Haskerwaul.Magma.Invertible
  -- * extended modules
  , module Haskerwaul.Magma.Invertible.Left
  , module Haskerwaul.Magma.Invertible.Right
  ) where

import Haskerwaul.Magma.Invertible.Left
import Haskerwaul.Magma.Invertible.Right

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/invertible+magma)
--
--  __NB__: Instances for this are automatically coalesced.
class (LeftInvertibleMagma c t a, RightInvertibleMagma c t a) =>
      InvertibleMagma c t a

instance (LeftInvertibleMagma c t a, RightInvertibleMagma c t a) =>
         InvertibleMagma c t a

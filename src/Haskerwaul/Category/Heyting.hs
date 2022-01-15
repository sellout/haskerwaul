{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Heyting
  ( module Haskerwaul.Category.Heyting
  , module Haskerwaul.Category.Coherent
  ) where

import Haskerwaul.Category.Coherent

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/Heyting+category)
class CoherentCategory c => HeytingCategory c

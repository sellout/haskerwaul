{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Regular
  ( module Haskerwaul.Category.Regular
  -- * extended modules
  , module Haskerwaul.Category.Complete.Finitely
  ) where

import Haskerwaul.Category.Complete.Finitely

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/regular+category)
class FinitelyCompleteCategory c => RegularCategory c

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Field
  ( module Haskerwaul.Field
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Skewfield
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Skewfield

-- | [nLab](https://ncatlab.org/nlab/show/field)
class (AbelianGroup c t (Multiplicative a), Skewfield c t a) => Field c t a

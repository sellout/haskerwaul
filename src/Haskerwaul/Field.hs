{-# language UndecidableSuperClasses #-}

module Haskerwaul.Field
  ( module Haskerwaul.Field
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Skewfield
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Skewfield

-- | https://ncatlab.org/nlab/show/field
class (AbelianGroup k t (Multiplicative a), Skewfield k t a) => Field k t a

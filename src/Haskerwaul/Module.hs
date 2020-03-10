{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Module
  ( module Haskerwaul.Module
  -- * extended modules
  , module Haskerwaul.Module.Left
  , module Haskerwaul.Module.Right
  ) where

import Haskerwaul.Module.Left
import Haskerwaul.Module.Right

-- | [nLab](https://ncatlab.org/nlab/show/module)
class (LeftModule c t r m, RightModule c t r m) => Module c t r m

instance (LeftModule c t r m, RightModule c t r m) => Module c t r m

scale :: Module c t r m => t r m `c` m
scale = leftScale

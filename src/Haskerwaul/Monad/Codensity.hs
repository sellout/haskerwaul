module Haskerwaul.Monad.Codensity
  ( module Haskerwaul.Monad.Codensity
  -- * extended modules
  , module Haskerwaul.Extension.Kan.Right
  ) where

import Haskerwaul.Extension.Kan.Right

-- | [nLab](https://ncatlab.org/nlab/show/codensity+monad)
type CodensityMonad c f = RightKanExtension c f f

module Haskerwaul.Embedding.Yoneda
  ( module Haskerwaul.Embedding.Yoneda
  , module Haskerwaul.Extension.Kan.Right
  ) where

import           Data.Functor.Identity (Identity)

import Haskerwaul.Extension.Kan.Right

-- | [nLab](https://ncatlab.org/nlab/show/Yoneda+embedding)
type Yoneda c = RightKanExtension c Identity

module Haskerwaul.Embedding.Yoneda
  ( module Haskerwaul.Embedding.Yoneda
  , module Haskerwaul.Extension.Kan.Right
  ) where

import           Data.Functor.Identity (Identity)

import Haskerwaul.Extension.Kan.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Yoneda+embedding)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Yoneda_lemma)
type Yoneda c = RightKanExtension c Identity

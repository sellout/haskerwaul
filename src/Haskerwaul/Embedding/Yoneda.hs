module Haskerwaul.Embedding.Yoneda
  ( module Haskerwaul.Embedding.Yoneda
  , module Haskerwaul.Extension.Kan.Right
  ) where

import           Data.Functor.Identity (Identity)

import Haskerwaul.Extension.Kan.Right

type Yoneda c = RightKanExtension c Identity

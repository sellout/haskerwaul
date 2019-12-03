module Haskerwaul.Cospan
  ( module Haskerwaul.Cospan
  -- * extended modules
  , module Haskerwaul.Span
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Span

-- | https://ncatlab.org/nlab/show/cospan
type Cospan k = Span (Op k)

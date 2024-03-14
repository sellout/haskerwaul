module Haskerwaul.Object.Initial
  ( module Haskerwaul.Object.Initial
  -- * extended modules
  , module Haskerwaul.Object.Terminal
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Object.Terminal

-- | [nLab](https://ncatlab.org/nlab/show/initial+object)
type InitialObject c = TerminalObject (Opposite c)

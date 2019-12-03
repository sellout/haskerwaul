module Haskerwaul.Object.Initial
  ( module Haskerwaul.Object.Initial
  -- * extended modules
  , module Haskerwaul.Object.Terminal
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Object.Terminal

type HasInitialObject c = HasTerminalObject (Opposite c)

type InitialObject c = TerminalObject (Opposite c)

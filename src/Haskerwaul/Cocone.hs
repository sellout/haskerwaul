module Haskerwaul.Cocone
  ( module Haskerwaul.Cocone
  -- * extended modules
  , module Haskerwaul.Cone
  ) where

import Haskerwaul.Category.Opposite
import Haskerwaul.Cone

type Cocone k = Cone (Op k)

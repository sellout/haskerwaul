module Haskerwaul.Object.NaturalNumbers where

import           Numeric.Natural (Natural)
import           Prelude (succ)

import Haskerwaul.Object.Terminal

-- | [nLab](https://ncatlab.org/nlab/show/natural+numbers+object)
class NaturalNumbersObject c n where
  z :: TerminalObject c `c` n
  s :: n `c` n

instance NaturalNumbersObject (->) Natural where
  z () = 0
  s = succ

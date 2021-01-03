module Haskerwaul.Object.NaturalNumbers where

import           Numeric.Natural (Natural)
import           Prelude (succ)

import Haskerwaul.Object.Terminal

-- | [nLab](https://ncatlab.org/nlab/show/natural+numbers+object)
class NaturalNumbersObject c where
  type NNO c
  z :: TerminalObject c `c` NNO c
  s :: NNO c `c` NNO c

instance NaturalNumbersObject (->) where
  type NNO (->) = Natural
  z () = 0
  s = succ

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Object.Integers where

import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Object.NaturalNumbers (NNO)
import Haskerwaul.Object.Terminal
import Prelude (Integer)
import qualified Prelude

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/integers+object)
class (CartesianClosedCategory c) => IntegersObject c where
  type Z c
  zero :: TerminalObject c `c` NNO c
  successor :: Z c `c` Z c
  negate :: Z c `c` Z c

predecessor :: (IntegersObject c) => Z c `c` Z c
predecessor = negate . successor . negate

adjacent :: (IntegersObject c) => Isomorphism c (Z c) (Z c)
adjacent = Iso successor predecessor

instance IntegersObject (->) where
  type Z (->) = Integer
  zero () = 0
  successor = Prelude.succ
  negate = Prelude.negate

{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Complete.Finitely
  ( module Haskerwaul.Category.Complete.Finitely,

    -- * extended modules
    module Haskerwaul.Category.LocallyCartesian,
    module Haskerwaul.Object.Terminal,
  )
where

import Haskerwaul.Category.LocallyCartesian
import Haskerwaul.Object.Terminal

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/finitely+complete+category)
--
--  __NB__: Instances for this are automatically coalesced.
class (LocallyCartesianCategory c, HasTerminalObject c) => FinitelyCompleteCategory c

instance (LocallyCartesianCategory c, HasTerminalObject c) => FinitelyCompleteCategory c

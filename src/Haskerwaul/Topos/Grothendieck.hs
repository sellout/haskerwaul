{-# language UndecidableSuperClasses #-}

module Haskerwaul.Topos.Grothendieck
  ( module Haskerwaul.Topos.Grothendieck
  -- * extended modules
  , module Haskerwaul.Topos.Elementary
  ) where

import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/Grothendieck+topos)
class ElementaryTopos c => GrothendieckTopos c

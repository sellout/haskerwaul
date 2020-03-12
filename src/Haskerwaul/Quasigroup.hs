{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup
  ( module Haskerwaul.Quasigroup
  -- * extended modules
  , module Haskerwaul.Quasigroup.Left
  , module Haskerwaul.Quasigroup.Right
  ) where

import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right

-- | [nLab](https://ncatlab.org/nlab/show/quasigroup)
--
--  __NB__: Instances for this are automatically coalesced.
class (LeftQuasigroup c t a, RightQuasigroup c t a) => Quasigroup c t a

instance (LeftQuasigroup c t a, RightQuasigroup c t a) => Quasigroup c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup
  ( module Haskerwaul.Quasigroup
  -- * extended modules
  , module Haskerwaul.Quasigroup.Left
  , module Haskerwaul.Quasigroup.Right
  ) where

import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right

-- | https://ncatlab.org/nlab/show/quasigroup
class (LeftQuasigroup k t a, RightQuasigroup k t a) => Quasigroup k t a

instance (LeftQuasigroup k t a, RightQuasigroup k t a) => Quasigroup k t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Meadow.NonInvolutive
  ( module Haskerwaul.Meadow.NonInvolutive
  -- * extended modules
  , module Haskerwaul.Meadow
  ) where

import Haskerwaul.Meadow

-- | https://www.sciencedirect.com/science/article/pii/S1570868314000652
class Meadow c t a => NonInvolutiveMeadow c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Field.Totalized.One
  ( module Haskerwaul.Field.Totalized.One
  -- * extended modules
  , module Haskerwaul.Field.Totalized
  , module Haskerwaul.Meadow.NonInvolutive
  ) where

import Haskerwaul.Field.Totalized
import Haskerwaul.Meadow.NonInvolutive

class (NonInvolutiveMeadow c t a, TotalizedField c t a) =>
      OneTotalizedField c t a

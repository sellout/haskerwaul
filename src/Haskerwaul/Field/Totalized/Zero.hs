{-# language UndecidableSuperClasses #-}

module Haskerwaul.Field.Totalized.Zero
  ( module Haskerwaul.Field.Totalized.Zero
  -- * extended modules
  , module Haskerwaul.Field.Totalized
  , module Haskerwaul.Meadow
  ) where

import Haskerwaul.Field.Totalized
import Haskerwaul.Meadow

-- | [The Rational Numbers as an Abstract Data Type](http://cs.swan.ac.uk/~csjvt/JVTPublications/JACMrationals%28published%29.pdf)
class (Meadow c t a, TotalizedField c t a) => ZeroTotalizedField c t a

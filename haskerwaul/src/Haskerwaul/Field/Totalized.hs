{-# language UndecidableSuperClasses #-}

module Haskerwaul.Field.Totalized
  ( module Haskerwaul.Field.Totalized
  -- * extended modules
  , module Haskerwaul.Field
  ) where

import Haskerwaul.Field

-- | [The Rational Numbers as an Abstract Data Type](http://cs.swan.ac.uk/~csjvt/JVTPublications/JACMrationals%28published%29.pdf)
class Field c t a => TotalizedField c t a

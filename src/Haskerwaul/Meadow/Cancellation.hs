{-# language UndecidableSuperClasses #-}

module Haskerwaul.Meadow.Cancellation
  ( module Haskerwaul.Meadow.Cancellation
  -- * extended modules
  , module Haskerwaul.Meadow
  ) where

import Haskerwaul.Meadow

-- | [Division by zero in non-involutive meadows](https://www.sciencedirect.com/science/article/pii/S1570868314000652)
--
-- = laws
--   [cancellation]: @x /= 0 => x * (`inverse` x) == 1@
class Meadow c t a => CancellationMeadow c t a


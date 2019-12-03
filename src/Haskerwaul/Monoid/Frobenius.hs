{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius
  ( module Haskerwaul.Monoid.Frobenius
  -- * extended modules
  , module Haskerwaul.Comonoid
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Comonoid
import Haskerwaul.Monoid

class (Monoid k t a, Comonoid k t a) => FrobeniusMonoid k t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius
  ( module Haskerwaul.Monoid.Frobenius
  -- * extended modules
  , module Haskerwaul.Comonoid
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Comonoid
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/Frobenius+algebra)
class (Monoid c t a, Comonoid c t a) => FrobeniusMonoid c t a

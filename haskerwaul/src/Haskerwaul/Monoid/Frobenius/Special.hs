{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Special
  ( module Haskerwaul.Monoid.Frobenius.Special
  -- * extended modules
  , module Haskerwaul.Monoid.Frobenius
  ) where

import Haskerwaul.Monoid.Frobenius

-- | [nLab](https://ncatlab.org/nlab/show/Frobenius+algebra#special_frobenius_algebras)
class FrobeniusMonoid c t a => SpecialFrobeniusMonoid c t a

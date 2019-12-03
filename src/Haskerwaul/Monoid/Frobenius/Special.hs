{-# language UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Frobenius.Special
  ( module Haskerwaul.Monoid.Frobenius.Special
  -- * extended modules
  , module Haskerwaul.Monoid.Frobenius
  ) where

import Haskerwaul.Monoid.Frobenius

-- | https://ncatlab.org/nlab/show/Frobenius+algebra#special_frobenius_algebras
class FrobeniusMonoid k t a => SpecialFrobeniusMonoid k t a

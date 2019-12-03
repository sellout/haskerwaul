{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup.Inverse where

import Haskerwaul.Quasigroup
import Haskerwaul.Semigroup

-- | https://ncatlab.org/nlab/show/inverse+semigroup
class (Quasigroup k t a, Semigroup k t a) => InverseSemigroup k t a

instance (Quasigroup k t a, Semigroup k t a) => InverseSemigroup k t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup.Inverse where

import Haskerwaul.Quasigroup
import Haskerwaul.Semigroup

-- | [nLab](https://ncatlab.org/nlab/show/inverse+semigroup)
class (Quasigroup c t a, Semigroup c t a) => InverseSemigroup c t a

instance (Quasigroup c t a, Semigroup c t a) => InverseSemigroup c t a

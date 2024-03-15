{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Commutative.Monus
  ( module Haskerwaul.Monoid.Commutative.Monus,

    -- * extended modules
    module Haskerwaul.Monoid.Commutative,
  )
where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Semiring.Pre.Near
import Numeric.Natural
import qualified Prelude as Base

class (CommutativeMonoid c t a) => CommutativeMonoidMonus c t a where
  -- | [nLab](https://ncatlab.org/nlab/show/monus)
  monus :: t a a `c` a

instance CommutativeMonoidMonus (->) (,) (Additive Natural) where
  monus (Add x, Add y) = Add (x Base.- y)

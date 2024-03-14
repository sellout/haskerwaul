{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Ring.Commutative
  ( module Haskerwaul.Ring.Commutative
  -- * extended modules
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Ring
  ) where

import Haskerwaul.Monoid.Commutative
import Haskerwaul.Ring

class (Ring c t a, CommutativeMonoid c t (Multiplicative a)) =>
      CommutativeRing c t a

instance (Ring c t a, CommutativeMonoid c t (Multiplicative a)) =>
         CommutativeRing c t a

{-# language UndecidableSuperClasses #-}

module Haskerwaul.Loop
  ( module Haskerwaul.Loop
  -- * extended modules
  , module Haskerwaul.Magma.Unital
  , module Haskerwaul.Quasigroup
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Magma.Unital
import Haskerwaul.Quasigroup

class (UnitalMagma c t a, Quasigroup c t a) => Loop c t a

instance (UnitalMagma c t a, Quasigroup c t a) => Loop c t a

leftInverse :: (c ~ (->), t ~ (,), Loop c t a) => Proxy t -> a `c` a
leftInverse t x = rightQuotient (unit t (), x)

rightInverse :: (c ~ (->), t ~ (,), Loop c t a) => Proxy t -> a `c` a
rightInverse t x = leftQuotient (x, unit t ())

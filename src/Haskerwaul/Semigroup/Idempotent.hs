{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup.Idempotent
  ( module Haskerwaul.Semigroup.Idempotent
  -- * extended modules
  , module Haskerwaul.Magma.Idempotent
  , module Haskerwaul.Semigroup
  ) where

import Haskerwaul.Magma.Idempotent
import Haskerwaul.Semigroup

-- |
--  __NB__: Instances for this are automatically coalesced.
class (IdempotentMagma c t a, Semigroup c t a) => IdempotentSemigroup c t a

instance (IdempotentMagma c t a, Semigroup c t a) => IdempotentSemigroup c t a

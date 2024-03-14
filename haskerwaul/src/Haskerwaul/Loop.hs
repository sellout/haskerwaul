{-# language TypeApplications
           , UndecidableSuperClasses #-}

module Haskerwaul.Loop
  ( module Haskerwaul.Loop
  -- * extended modules
  , module Haskerwaul.Magma.Unital
  , module Haskerwaul.Quasigroup
  ) where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Magma.Unital
import Haskerwaul.Quasigroup

-- | [Wikipedia](https://en.wikipedia.org/wiki/Quasigroup#Loops)
--
--  __NB__: Instances for this are automatically coalesced.
class (UnitalMagma c t a, Quasigroup c t a) => Loop c t a

instance (UnitalMagma c t a, Quasigroup c t a) => Loop c t a

leftInverse :: forall c a
             . (CartesianMonoidalCategory c, Loop c (Prod c) a)
            => a `c` a
leftInverse =
  rightQuotient
  . first @c (Proxy :: Proxy c) (rightQuotient . diagonal)
  . diagonal

rightInverse :: forall c a
              . (CartesianMonoidalCategory c, Loop c (Prod c) a)
             => a `c` a
rightInverse =
  leftQuotient
  . second @c @c (Proxy :: Proxy c) (leftQuotient . diagonal)
  . diagonal

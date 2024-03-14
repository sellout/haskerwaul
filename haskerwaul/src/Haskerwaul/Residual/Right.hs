{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Residual.Right
  ( module Haskerwaul.Residual.Right,
  )
where

import Data.Kind (Type)
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/residual)
class (MonoidalCategory c t, Ob c x, Ob c y, Ob c (RightRes c t x y)) => RightResidual (c :: ok -> ok -> Type) t x y where
  type RightRes c t x y :: ok
  rev :: t (RightRes c t x y) x `c` y
  rcur :: (Ob c a) => t a x `c` y -> a `c` RightRes c t x y

-- | "Any monoidal closed category has all right residuals."
--   ---[nLab](https://ncatlab.org/nlab/show/residual#examples)
instance
  ( ClosedMonoidalCategory c t,
    MonoidalCategory c t, -- I don't understand why this constraint isn't implied
    Ob c x,
    Ob c y,
    Ob c (InternalHom c x y)
  ) =>
  RightResidual c t x y
  where
  type RightRes c t x y = InternalHom c x y
  rev = apply
  rcur = curry

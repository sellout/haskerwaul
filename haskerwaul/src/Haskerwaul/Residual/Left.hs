{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Residual.Left
  ( module Haskerwaul.Residual.Left,
  )
where

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Residual.Right

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/residual)
class (MonoidalCategory c t, Ob c x, Ob c y, Ob c (LeftRes c t x y)) => LeftResidual c t x y where
  type LeftRes c t x y :: ok
  lev :: t x (LeftRes c t x y) `c` y
  lcur :: (Ob c a) => t x a `c` y -> a `c` LeftRes c t x y

-- | This means you only need to instantiate a `RightResidual` to define a
--  `Haskerwaul.Residual.Residual` for a `SymmetricMonoidalCategory`.
instance
  {-# OVERLAPPABLE #-}
  ( SymmetricMonoidalCategory c t,
    MonoidalCategory c t,
    Ob c x,
    Ob c y, -- how are these not implied?
    RightResidual c t x y,
    Ob c (RightRes c t x y)
  ) =>
  LeftResidual c t x y
  where
  type LeftRes c t x y = RightRes c t x y
  lev = rev . to braid
  lcur f = rcur (f . to braid)

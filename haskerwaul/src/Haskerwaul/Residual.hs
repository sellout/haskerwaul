{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Residual
  ( module Haskerwaul.Residual,

    -- * extended modules
    module Haskerwaul.Residual.Left,
    module Haskerwaul.Residual.Right,
  )
where

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Residual.Left
import Haskerwaul.Residual.Right

-- | This definition is a bit backward. It's more that in a symmetric monoidal
--   category, either residual implies the other.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/residual)
--
--  __NB__: Instances for this are automatically coalesced
class
  (SymmetricMonoidalCategory c t, LeftResidual c t x y, RightResidual c t x y) =>
  Residual c t x y
  where
  type Res c t x y :: ok

instance
  (SymmetricMonoidalCategory c t, LeftResidual c t x y, RightResidual c t x y) =>
  Residual c t x y
  where
  type Res c t x y = RightRes c t x y

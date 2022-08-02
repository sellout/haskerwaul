{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Endomorphism where

import Data.Proxy (Proxy (..))
import Haskerwaul.Group
import Haskerwaul.Semicategory
import Haskerwaul.Transformation.Dinatural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/endomorphism)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Endomorphism)
newtype Endomorphism c x = Endo (x `c` x)

-- | `Endomorphism`s in any `Category`-like thing can be seen as "proper"
--   versions of the same structure in the enriching Cartesian category.
instance (Magma (DinaturalTransformation (->)) Procompose c) => Magma (->) (,) (Endomorphism c x) where
  op (Endo f, Endo g) = Endo (f . g)

-- | `Endomorphism`s in any `Category`-like thing can be seen as "proper"
--   versions of the same structure in the enriching Cartesian category.
instance (Semigroup (DinaturalTransformation (->)) Procompose c) => Semigroup (->) (,) (Endomorphism c x)

-- | `Endomorphism`s in any `Category`-like thing can be seen as "proper"
--   versions of the same structure in the enriching Cartesian category.
instance (UnitalMagma (DinaturalTransformation (->)) Procompose c) => UnitalMagma (->) (,) (Endomorphism c x) where
  unit Proxy () = Endo id

-- | `Endomorphism`s in any `Category`-like thing can be seen as "proper"
--   versions of the same structure in the enriching Cartesian category.
instance (LeftQuasigroup (DinaturalTransformation (->)) Procompose c) => LeftQuasigroup (->) (,) (Endomorphism c x) where
  leftQuotient (Endo f, Endo g) = Endo (runDT leftQuotient (Procompose f g))

-- | `Endomorphism`s in any `Category`-like thing can be seen as "proper"
--   versions of the same structure in the enriching Cartesian category.
instance (RightQuasigroup (DinaturalTransformation (->)) Procompose c) => RightQuasigroup (->) (,) (Endomorphism c x) where
  rightQuotient (Endo f, Endo g) = Endo (runDT rightQuotient (Procompose f g))

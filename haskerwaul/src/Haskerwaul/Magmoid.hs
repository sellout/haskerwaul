{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
--  __NB__: This module includes `UnitalMagma` and `Quasigroup` instances for
--          various categories (even though those would fit better in
--         "Haskerwaul.Category" and "Haskerwaul.Groupoid", respectively)
--          because it avoids them being orphan instances, since `Procompose` is
--          defined here.
module Haskerwaul.Magmoid
  ( module Haskerwaul.Magmoid,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import qualified Control.Category as Base
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import Haskerwaul.Magma
import Haskerwaul.Magma.Unital
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Semigroup
import Haskerwaul.Transformation.Dinatural

-- | Composition of profunctors, a tensor in profunctor categories.
--
--  __TODO__: Is this any more general than the version in profunctors? If not,
--            maybe we should depend on that library (although maybe that
--            introduces too many other conflicting definitions).
data Procompose c d a b = forall z. Procompose (z `c` b) (a `d` z)

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/magmoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type Magmoid = Magma (DinaturalTransformation (->)) Procompose

-- | Just a bit of sugar over `op`, when it's used categorcally.
(.) :: (Magmoid c) => z `c` b -> a `c` z -> a `c` b
f . g = runDT op (Procompose f g)

-- | The correct unit here is the Hom functor, but I don't know how to define
--   that, and this approach seems to work well enough for now.
instance MonoidalCategory' (DinaturalTransformation (->)) Procompose where
  type Unit (DinaturalTransformation (->)) Procompose = (:~:)

-- | All `Base.Category` instances are also `Magmoid` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  Magma (DinaturalTransformation (->)) Procompose c
  where
  op = DT (\(Procompose f g) -> f Base.. g)

-- | All `Base.Category` instances are also `FlexibleMagmoid` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose c

-- | All `Base.Category` instances are also `Semicategory` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  Semigroup (DinaturalTransformation (->)) Procompose c

-- | All `Base.Category` instances are also `UnitalMagmoid` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose c
  where
  unit Proxy = DT (\Refl -> Base.id)

-- | If /C/ is a `Magmoid`, then so are /C/-valued bifunctors.
instance
  (Magmoid c) =>
  Magma (DinaturalTransformation (->)) Procompose (DinaturalTransformation c)
  where
  op = DT (\(Procompose (DT f) (DT g)) -> DT (f . g))

-- | If /C/ is a `FlexibleMagmoid`, then so are /C/-valued bifunctors.
instance
  (FlexibleMagma (DinaturalTransformation (->)) Procompose c) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose (DinaturalTransformation c)

-- | If /C/ is a `Semicategory`, then so are /C/-valued bifunctors.
instance
  (Semigroup (DinaturalTransformation (->)) Procompose c) =>
  Semigroup (DinaturalTransformation (->)) Procompose (DinaturalTransformation c)

-- | If /C/ is a `UnitalMagmoid`, then so are /C/-valued bifunctors.
instance
  (UnitalMagma (DinaturalTransformation (->)) Procompose c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (DinaturalTransformation c)
  where
  unit Proxy = DT (\Refl -> DT (runDT (unit (Proxy :: Proxy Procompose)) Refl))

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Magma (DinaturalTransformation (->)) Procompose (:~:) where
  op = DT (\(Procompose Refl Refl) -> Refl)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance FlexibleMagma (DinaturalTransformation (->)) Procompose (:~:)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Semigroup (DinaturalTransformation (->)) Procompose (:~:)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance UnitalMagma (DinaturalTransformation (->)) Procompose (:~:) where
  unit Proxy = DT (runDT (unit (Proxy :: Proxy Procompose)) Refl)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance LeftQuasigroup (DinaturalTransformation (->)) Procompose (:~:) where
  leftQuotient = DT (\(Procompose Refl Refl) -> Refl)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance RightQuasigroup (DinaturalTransformation (->)) Procompose (:~:) where
  rightQuotient = DT (\(Procompose Refl Refl) -> Refl)

-- -- | If /C/ and /C'/ are `Magmoid` instances, then so is their product.
-- instance ( Magma (DinaturalTransformation (->)) Procompose c
--          , Magma (DinaturalTransformation (->)) Procompose c') =>
--          Magma (DinaturalTransformation (->)) Procompose (c :**: c') where
--   op = DT (\(Procompose (NT (ProdC f)) (NT (ProdC g))) -> NT (f . g))

-- -- | If /C/ and /C'/ are `Semicategory` instances, then so is their product.
-- instance ( Semigroup (DinaturalTransformation (->)) Procompose c
--          , Semigroup (DinaturalTransformation (->)) Procompose c') =>
--          Semigroup (DinaturalTransformation (->)) Procompose (c :**: c')

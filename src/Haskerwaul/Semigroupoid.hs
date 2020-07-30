-- |
--  __NB__: This module includes `UnitalMagma` and `Quasigroup` instances for
--          various categories (even though those would fit better in
--         "Haskerwaul.Category" and "Haskerwaul.Groupoid", respectively)
--          because it avoids them being orphan instances, since `Procompose` is
--          defined here.
module Haskerwaul.Semigroupoid
  ( module Haskerwaul.Semigroupoid
  -- * extended modules
  , module Haskerwaul.Semigroup
  ) where

import qualified Control.Category as Base
import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Magma.Unital
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Semigroup
import Haskerwaul.Transformation.Natural

-- | Composition of profunctors, a tensor in profunctor categories.
--
--  __TODO__: Is this any more general than the version in profunctors? If not,
--            maybe we should depend on that library (although maybe that
--            introduces too many other conflicting definitions).
data Procompose c d a b = forall z. Procompose (z `c` b) (a `d` z)

-- |
-- = references
--
-- - [Wikipedia](https://en.wikipedia.org/wiki/Semigroupoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type Semigroupoid = Semigroup (NaturalTransformation2 (->)) Procompose

-- | Just a bit of sugar over `op`, when it's used categorcally.
(.) :: Magma (NaturalTransformation2 (->)) Procompose c
    => z `c` b -> a `c` z -> a `c` b
f . g = runNT2 op (Procompose f g)

-- | Just a bit of sugar over `unit`, when it's used categorcally.
id :: UnitalMagma (NaturalTransformation2 (->)) Procompose c => a `c` a
id = runNT2 (unit (Proxy :: Proxy Procompose)) Refl

-- | The correct unit here is the Hom functor, but I don't know how to define
--   that, and this approach seems to work well enough for now.
instance MonoidalCategory' (NaturalTransformation2 (->)) Procompose where
  type Unit (NaturalTransformation2 (->)) Procompose = (:~:)

-- | All `Base.Category` instances are also `Semigroupoid` instances.
instance {-# overlappable #-} Base.Category c =>
                              Magma (NaturalTransformation2 (->)) Procompose c where
  op = NT2 (\(Procompose f g) -> f . g)

-- | All `Base.Category` instances are also `Semigroupoid` instances.
instance {-# overlappable #-} Base.Category c =>
                              Semigroup (NaturalTransformation2 (->)) Procompose c

-- | All `Base.Category` instances are also `Haskerwaul.Category` instances.
instance {-# overlappable #-} Base.Category c =>
                              UnitalMagma (NaturalTransformation2 (->)) Procompose c where
  unit Proxy = NT2 (\Refl -> Base.id)

-- | If /C/ is a `Semigroupoid`, then so are /C/-valued functors.
instance Magma (NaturalTransformation2 (->)) Procompose c =>
         Magma (NaturalTransformation2 (->)) Procompose (NaturalTransformation c) where
  op = NT2 (\(Procompose (NT f) (NT g)) -> NT (f . g))

-- | If /C/ is a `Semigroupoid`, then so are /C/-valued functors.
instance Semigroup (NaturalTransformation2 (->)) Procompose c =>
         Semigroup (NaturalTransformation2 (->)) Procompose (NaturalTransformation c)

-- | If /C/ is a `Category`, then so are /C/-valued functors.
instance UnitalMagma (NaturalTransformation2 (->)) Procompose c =>
         UnitalMagma (NaturalTransformation2 (->)) Procompose (NaturalTransformation c) where
  unit Proxy = NT2 (\Refl -> NT id)

-- | If /C/ is a `Semigroupoid`, then so are /C/-valud bifunctors.
instance Magma (NaturalTransformation2 (->)) Procompose c =>
         Magma (NaturalTransformation2 (->)) Procompose (NaturalTransformation2 c) where
  op = NT2 (\(Procompose (NT2 f) (NT2 g)) -> NT2 (f . g))

-- | If /C/ is a `Semigroupoid`, then so are /C/-valud bifunctors.
instance Semigroup (NaturalTransformation2 (->)) Procompose c =>
         Semigroup (NaturalTransformation2 (->)) Procompose (NaturalTransformation2 c)

-- | If /C/ is a `Category`, then so are /C/-valud bifunctors.
instance UnitalMagma (NaturalTransformation2 (->)) Procompose c =>
         UnitalMagma (NaturalTransformation2 (->)) Procompose (NaturalTransformation2 c) where
  unit Proxy = NT2 (\Refl -> NT2 id)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Magma (NaturalTransformation2 (->)) Procompose (:~:) where
  op = NT2 (\(Procompose Refl Refl) -> Refl)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Semigroup (NaturalTransformation2 (->)) Procompose (:~:)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance UnitalMagma (NaturalTransformation2 (->)) Procompose (:~:) where
  unit Proxy = NT2 id

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance LeftQuasigroup (NaturalTransformation2 (->)) Procompose (:~:) where
  leftQuotient = NT2 (\(Procompose Refl Refl) -> Refl)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance RightQuasigroup (NaturalTransformation2 (->)) Procompose (:~:) where
  rightQuotient = NT2 (\(Procompose Refl Refl) -> Refl)

-- -- | If /C/ and /C'/ are `Semigroupoid` instances, then so is their product.
-- instance ( Magma (NaturalTransformation2 (->)) Procompose c
--          , Magma (NaturalTransformation2 (->)) Procompose c') =>
--          Magma (NaturalTransformation2 (->)) Procompose (c :**: c') where
--   op = NT2 (\(Procompose (NT (ProdC f)) (NT (ProdC g))) -> NT (f . g))

-- -- | If /C/ and /C'/ are `Semigroupoid` instances, then so is their product.
-- instance ( Semigroup (NaturalTransformation2 (->)) Procompose c
--          , Semigroup (NaturalTransformation2 (->)) Procompose c') =>
--          Semigroup (NaturalTransformation2 (->)) Procompose (c :**: c')

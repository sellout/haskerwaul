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

-- | A categorical tensor.
data CProd c d a b = forall z. CProd (z `c` b) (a `d` z)

-- | [Wikipedia](https://en.wikipedia.org/wiki/Semigroupoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type Semigroupoid = Semigroup (NaturalTransformation2 (->)) CProd

-- | Just a bit of sugar over `op`, when it's used categorcally.
(.) :: Magma (NaturalTransformation2 (->)) CProd c
    => z `c` b -> a `c` z -> a `c` b
f . g = runNT2 op (CProd f g)

-- | Just a bit of sugar over `unit`, when it's used categorcally.
id :: UnitalMagma (NaturalTransformation2 (->)) CProd c => a `c` a
id = runNT2 (unit (Proxy :: Proxy CProd)) Refl

-- | I don't think this is the correct unit, but it seems to work.
instance MonoidalCategory' (NaturalTransformation2 (->)) CProd where
  type Unit (NaturalTransformation2 (->)) CProd = (:~:)

-- * `Magma` instances

instance {-# overlappable #-} Base.Category c =>
                              Magma (NaturalTransformation2 (->)) CProd c where
  op = NT2 (\(CProd f g) -> f . g)

instance Magma (NaturalTransformation2 (->)) CProd c =>
         Magma (NaturalTransformation2 (->)) CProd (NaturalTransformation c) where
  op = NT2 (\(CProd (NT f) (NT g)) -> NT (f . g))

instance Magma (NaturalTransformation2 (->)) CProd c =>
         Magma (NaturalTransformation2 (->)) CProd (NaturalTransformation2 c) where
  op = NT2 (\(CProd (NT2 f) (NT2 g)) -> NT2 (f . g))

-- | a discrete groupoid --
--  [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Magma (NaturalTransformation2 (->)) CProd (:~:) where
  op = NT2 (\(CProd Refl Refl) -> Refl)

-- instance ( Magma (NaturalTransformation2 (->)) CProd c
--          , Magma (NaturalTransformation2 (->)) CProd c') =>
--          Magma (NaturalTransformation2 (->)) CProd (c :**: c') where
--   op = NT2 (\(CProd (NT (ProdC f)) (NT (ProdC g))) -> NT (f . g))

-- * `Semigroup` instances

instance {-# overlappable #-} Base.Category c =>
                              Semigroup (NaturalTransformation2 (->)) CProd c

instance Semigroup (NaturalTransformation2 (->)) CProd c =>
         Semigroup (NaturalTransformation2 (->)) CProd (NaturalTransformation c)

instance Semigroup (NaturalTransformation2 (->)) CProd c =>
         Semigroup (NaturalTransformation2 (->)) CProd (NaturalTransformation2 c)

-- | a discrete groupoid --
--  [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Semigroup (NaturalTransformation2 (->)) CProd (:~:)

-- instance ( Semigroup (NaturalTransformation2 (->)) CProd c
--          , Semigroup (NaturalTransformation2 (->)) CProd c') =>
--          Semigroup (NaturalTransformation2 (->)) CProd (c :**: c')

-- * `UnitalMagma` instances

instance {-# overlappable #-} Base.Category c =>
                              UnitalMagma (NaturalTransformation2 (->)) CProd c where
  unit Proxy = NT2 (\Refl -> Base.id)

instance UnitalMagma (NaturalTransformation2 (->)) CProd c =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (NaturalTransformation c) where
  unit Proxy = NT2 (\Refl -> NT id)

instance UnitalMagma (NaturalTransformation2 (->)) CProd c =>
         UnitalMagma (NaturalTransformation2 (->)) CProd (NaturalTransformation2 c) where
  unit Proxy = NT2 (\Refl -> NT2 id)

-- | a discrete groupoid --
--  [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance UnitalMagma (NaturalTransformation2 (->)) CProd (:~:) where
  unit Proxy = NT2 id

-- | a discrete groupoid --
--  [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance LeftQuasigroup (NaturalTransformation2 (->)) CProd (:~:) where
  leftQuotient = NT2 (\(CProd Refl Refl) -> Refl)

-- | a discrete groupoid --
--  [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance RightQuasigroup (NaturalTransformation2 (->)) CProd (:~:) where
  rightQuotient = NT2 (\(CProd Refl Refl) -> Refl)

module Haskerwaul.Isomorphism where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Magma.Unital
import Haskerwaul.Object
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/isomorphism)
data Isomorphism c a b = Iso { to :: a `c` b, from :: b `c` a }

type instance Ob (Isomorphism c) = Ob c

instance Magma (NaturalTransformation2 (->)) Procompose c =>
         Magma (NaturalTransformation2 (->)) Procompose (Isomorphism c) where
  op = NT2 (\(Procompose f g) -> Iso (to f . to g) (from g . from f))

instance Semigroup (NaturalTransformation2 (->)) Procompose c =>
         Semigroup (NaturalTransformation2 (->)) Procompose (Isomorphism c)

instance UnitalMagma (NaturalTransformation2 (->)) Procompose c =>
         UnitalMagma (NaturalTransformation2 (->)) Procompose (Isomorphism c) where
  unit Proxy = NT2 (\Refl -> Iso id id)

instance MonoidalCategory' c t => MonoidalCategory' (Isomorphism c) t where
  type Unit (Isomorphism c) t = Unit c t

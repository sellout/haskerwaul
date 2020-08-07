module Haskerwaul.Isomorphism where

import           Data.Constraint.Deferrable ((:~:)(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Magma.Unital
import Haskerwaul.Object
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Dinatural

-- | [nLab](https://ncatlab.org/nlab/show/isomorphism)
data Isomorphism c a b = Iso { to :: a `c` b, from :: b `c` a }

type instance Ob (Isomorphism c) = Ob c

instance Magma (DinaturalTransformation (->)) Procompose c =>
         Magma (DinaturalTransformation (->)) Procompose (Isomorphism c) where
  op = DT (\(Procompose f g) -> Iso (to f . to g) (from g . from f))

instance Semigroup (DinaturalTransformation (->)) Procompose c =>
         Semigroup (DinaturalTransformation (->)) Procompose (Isomorphism c)

instance UnitalMagma (DinaturalTransformation (->)) Procompose c =>
         UnitalMagma (DinaturalTransformation (->)) Procompose (Isomorphism c) where
  unit Proxy = DT (\Refl -> Iso id id)

instance MonoidalCategory' c t => MonoidalCategory' (Isomorphism c) t where
  type Unit (Isomorphism c) t = Unit c t

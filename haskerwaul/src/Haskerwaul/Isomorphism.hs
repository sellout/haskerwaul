{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Haskerwaul.Isomorphism where

import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal'
import Haskerwaul.Magma.Unital
import Haskerwaul.Object
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Dinatural

-- | [nLab](https://ncatlab.org/nlab/show/isomorphism)
data Isomorphism c a b = Iso {to :: a `c` b, from :: b `c` a}

reverse :: Isomorphism c a b -> Isomorphism c b a
reverse iso = Iso (from iso) (to iso)

type instance Ob (Isomorphism c) = Ob c

instance
  (Magma (DinaturalTransformation (->)) Procompose c) =>
  Magma (DinaturalTransformation (->)) Procompose (Isomorphism c)
  where
  op = DT (\(Procompose f g) -> Iso (to f . to g) (from g . from f))

instance
  (Semigroup (DinaturalTransformation (->)) Procompose c) =>
  Semigroup (DinaturalTransformation (->)) Procompose (Isomorphism c)

instance
  (UnitalMagma (DinaturalTransformation (->)) Procompose c) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (Isomorphism c)
  where
  unit Proxy = DT (\Refl -> Iso id id)

instance
  (Bifunctor c c c f) =>
  Bifunctor (Isomorphism c) (Isomorphism c) (Isomorphism c) f
  where
  bimap f g = Iso (bimap (to f) (to g)) (bimap (from f) (from g))

instance (MonoidalCategory' c t) => MonoidalCategory' (Isomorphism c) t where
  type Unit (Isomorphism c) t = Unit c t

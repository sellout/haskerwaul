{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Semigroupal
  ( module Haskerwaul.Category.Semigroupal,

    -- * extended modules
    module Haskerwaul.Category,
  )
where

import Data.Constraint (Class (..), Dict (..), refl, trans, (***), (:-) (..), (:=>) (..))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

class (Category c, Bifunctor c c c t) => SemigroupalCategory c t where
  assoc :: (Ob c x, Ob c y, Ob c z) => Isomorphism c (t x (t y z)) (t (t x y) z)

instance SemigroupalCategory (->) (,) where
  assoc = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

instance SemigroupalCategory (->) Either where
  assoc =
    Iso
      ( \case
          Left a -> Left (Left a)
          Right (Left b) -> Left (Right b)
          Right (Right c) -> Right c
      )
      ( \case
          Left (Left a) -> Left a
          Left (Right b) -> Right (Left b)
          Right c -> Right (Right c)
      )

-- | Every functor category is additionally semigroupal in all the ways that the
--   destination category is.
instance
  (d ~ (->), SemigroupalCategory d dt) =>
  SemigroupalCategory (NaturalTransformation c d) (FTensor dt)
  where
  assoc =
    Iso
      (NT (FTensor . first p FTensor . to assoc . second p lowerFTensor . lowerFTensor))
      (NT (FTensor . second p FTensor . from assoc . first p lowerFTensor . lowerFTensor))
    where
      p :: Proxy d
      p = Proxy

instance
  (c ~ (->), SemigroupalCategory c t) =>
  SemigroupalCategory (DinaturalTransformation c) (BTensor t)
  where
  assoc =
    Iso
      (DT (BTensor . first p BTensor . to assoc . second p lowerBTensor . lowerBTensor))
      (DT (BTensor . second p BTensor . from assoc . first p lowerBTensor . lowerBTensor))
    where
      p :: Proxy c
      p = Proxy

instance (SemigroupalCategory c t) => SemigroupalCategory (Isomorphism c) t where
  assoc = Iso assoc (reverse assoc)

instance SemigroupalCategory (:-) Combine where
  assoc =
    Iso
      (trans ins (trans (ins *** refl) (trans (Sub Dict) (trans (refl *** cls) cls))))
      (trans ins (trans (refl *** ins) (trans (Sub Dict) (trans (cls *** refl) cls))))

instance SemigroupalCategory (NaturalTransformation c (:-)) CFProd where
  assoc = Iso (NT (Sub Dict)) (NT (Sub Dict))

instance SemigroupalCategory (DinaturalTransformation (->)) Procompose where
  assoc =
    Iso
      (DT (\(Procompose f (Procompose g h)) -> Procompose (Procompose f g) h))
      (DT (\(Procompose (Procompose f g) h) -> Procompose f (Procompose g h)))

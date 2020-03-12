{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Semigroupal
  ( module Haskerwaul.Category.Semigroupal
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import           Data.Constraint ((***), (:-)(..), (:=>)(..), Class(..), Dict(..), refl, trans)
import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

class (Category c, TOb (Ob c) t, Bifunctor c c c t) =>
      SemigroupalCategory c t where
  assoc :: (Ob c x, Ob c y, Ob c z) => Isomorphism c (t x (t y z)) (t (t x y) z)

instance SemigroupalCategory c t => SemigroupalCategory (Opposite c) t where
  assoc = runNT2 (to isomorphismOp) assoc

instance SemigroupalCategory (->) (,) where
  assoc = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

instance SemigroupalCategory (->) Either where
  assoc =
    Iso
    (\case
        Left a -> Left (Left a)
        Right (Left b) -> Left (Right b)
        Right (Right c) -> Right c)
    (\case
        Left (Left a) -> Left a
        Left (Right b) -> Right (Left b)
        Right c -> Right (Right c))

-- | Every functor category is additionally semigroupal in all the ways that the
--   destination category is.
instance (c ~ (->), SemigroupalCategory c t) =>
         SemigroupalCategory (NaturalTransformation c) (FTensor t) where
  assoc =
    Iso
    (NT (FTensor . first p FTensor . to assoc . second p lowerFTensor . lowerFTensor))
    (NT (FTensor . second p FTensor . from assoc . first p lowerFTensor . lowerFTensor))
   where
    p :: Proxy c
    p = Proxy

instance (c ~ (->), SemigroupalCategory c t) =>
         SemigroupalCategory (NaturalTransformation2 c) (BTensor t) where
  assoc =
    Iso
    (NT2 (BTensor . first p BTensor . to assoc . second p lowerBTensor . lowerBTensor))
    (NT2 (BTensor . second p BTensor . from assoc . first p lowerBTensor . lowerBTensor))
   where
    p :: Proxy c
    p = Proxy

instance SemigroupalCategory (:-) Combine where
  assoc =
    Iso
    (trans ins (trans (ins *** refl) (trans (Sub Dict) (trans (refl *** cls) cls))))
    (trans ins (trans (refl *** ins) (trans (Sub Dict) (trans (cls *** refl) cls))))

instance SemigroupalCategory (NaturalTransformation (:-)) CFProd where
  assoc = Iso (NT (Sub Dict)) (NT (Sub Dict))

instance SemigroupalCategory (NaturalTransformation2 (->)) CProd where
  assoc =
    Iso
    (NT2 (\(CProd f (CProd g h)) -> CProd (CProd f g) h))
    (NT2 (\(CProd (CProd f g) h) -> CProd f (CProd g h)))

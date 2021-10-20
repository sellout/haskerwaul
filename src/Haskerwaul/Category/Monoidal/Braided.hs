{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Braided
  ( module Haskerwaul.Category.Monoidal.Braided
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Either (Either(..))
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/braided+monoidal+category)
--
class MonoidalCategory c t => BraidedMonoidalCategory c t where
  braid :: (Ob c a, Ob c b) => Isomorphism c (t a b) (t b a)

instance BraidedMonoidalCategory (->) (,) where
  braid = Iso Base.swap Base.swap

instance BraidedMonoidalCategory (->) Either where
  braid = Iso
    (\case
        Left a -> Right a
        Right b -> Left b)
    (\case
        Left a -> Right a
        Right b -> Left b)

instance (d ~ (->), dt ~ (,), BraidedMonoidalCategory d dt) =>
         BraidedMonoidalCategory (NaturalTransformation c d) (FTensor dt) where
  braid =
    Iso
    (NT (FTensor . to braid . lowerFTensor))
    (NT (FTensor . from braid . lowerFTensor))

instance BraidedMonoidalCategory (:-) Combine where
  braid = Iso (Sub Dict) (Sub Dict)

instance BraidedMonoidalCategory (NaturalTransformation c (:-)) CFProd where
  braid = Iso (NT (Sub Dict)) (NT (Sub Dict))

instance (d ~ (->), BraidedMonoidalCategory d dt) =>
         BraidedMonoidalCategory (DinaturalTransformation d) (BTensor dt) where
  braid =
    Iso
    (DT (BTensor . to braid . lowerBTensor))
    (DT (BTensor . from braid . lowerBTensor))

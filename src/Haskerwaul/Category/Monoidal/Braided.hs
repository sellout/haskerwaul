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
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/braided+monoidal+category)
class MonoidalCategory c t => BraidedMonoidalCategory c t where
  braid :: (Ob c a, Ob c b) => t a b `c` t b a

instance BraidedMonoidalCategory (->) (,) where
  braid = Base.swap

instance BraidedMonoidalCategory (->) Either where
  braid = \case
    Left a -> Right a
    Right b -> Left b

instance (d ~ (->), BraidedMonoidalCategory d dt) =>
         BraidedMonoidalCategory (NaturalTransformation c d) (FTensor dt) where
  braid = NT (FTensor . braid . lowerFTensor)

instance BraidedMonoidalCategory (:-) Combine where
  braid = Sub Dict

instance BraidedMonoidalCategory (NaturalTransformation c (:-)) CFProd where
  braid = NT (Sub Dict)

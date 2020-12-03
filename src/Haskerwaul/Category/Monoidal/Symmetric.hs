{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Symmetric
  ( module Haskerwaul.Category.Monoidal.Symmetric
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Braided
  ) where

import           Data.Constraint ((:-))
import           Data.Either (Either(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/symmetric+monoidal+category)
class BraidedMonoidalCategory c t => SymmetricMonoidalCategory c t

symmetric :: (SymmetricMonoidalCategory c t, Ob c a, Ob c b)
          => Isomorphism c (t a b) (t b a)
symmetric = Iso braid braid

instance SymmetricMonoidalCategory (->) (,)

instance SymmetricMonoidalCategory (->) Either

instance (d ~ (->), SymmetricMonoidalCategory d dt) =>
         SymmetricMonoidalCategory (NaturalTransformation c d) (FTensor dt)

instance SymmetricMonoidalCategory (:-) Combine

instance SymmetricMonoidalCategory (NaturalTransformation c (:-)) CFProd

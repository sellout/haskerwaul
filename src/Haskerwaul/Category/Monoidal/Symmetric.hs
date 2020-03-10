{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Symmetric
  ( module Haskerwaul.Category.Monoidal.Symmetric
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Braided
  ) where

import           Data.Either (Either(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Category.Opposite
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full

-- | [nLab](https://ncatlab.org/nlab/show/symmetric+monoidal+category)
class BraidedMonoidalCategory c t => SymmetricMonoidalCategory c t

symmetric :: (SymmetricMonoidalCategory c t, Ob c a, Ob c b)
          => Isomorphism c (t a b) (t b a)
symmetric = Iso braid braid

instance SymmetricMonoidalCategory (->) (,)

instance SymmetricMonoidalCategory (->) Either

instance SymmetricMonoidalCategory c t =>
         SymmetricMonoidalCategory (Opposite c) t

instance (SymmetricMonoidalCategory c t, TOb ob t, ob (Unit c t)) =>
         SymmetricMonoidalCategory (FullSubcategory ob c) t

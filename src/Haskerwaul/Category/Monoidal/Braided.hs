{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Braided where

import           Data.Either (Either(..))
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Opposite
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full

-- | https://ncatlab.org/nlab/show/braided+monoidal+category
class MonoidalCategory k t => BraidedMonoidalCategory k t where
  braid :: (Ob k a, Ob k b) => t a b `k` t b a

instance BraidedMonoidalCategory (->) (,) where
  braid = Base.swap

instance BraidedMonoidalCategory (->) Either where
  braid = \case
    Left a -> Right a
    Right b -> Left b

instance BraidedMonoidalCategory k t =>
         BraidedMonoidalCategory (Opposite k) t where
  braid = Opposite braid

instance (BraidedMonoidalCategory k t, TOb c t, c (Unit k t)) =>
         BraidedMonoidalCategory (FullSubcategory c k) t where
  braid = FS braid

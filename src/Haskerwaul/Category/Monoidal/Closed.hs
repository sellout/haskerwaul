{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Closed
  ( module Haskerwaul.Category.Monoidal.Closed
  -- * extended modules
  , module Haskerwaul.Category.Closed
  , module Haskerwaul.Category.Monoidal
  ) where

import qualified Data.Function as Base hiding ((.), id)
import           Data.Proxy (Proxy (..))
import qualified Data.Tuple as Base

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Closed
import Haskerwaul.Category.Monoidal
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/closed+monoidal+category)
class (ClosedCategory c, MonoidalCategory c t) =>
      ClosedMonoidalCategory c t where
  apply :: (Ob c a, Ob c b) => t (Exp c a b) a `c` b

instance ClosedMonoidalCategory (->) (,) where
  apply = Base.uncurry (Base.$)

instance (c ~ (->), ClosedMonoidalCategory c t) =>
         ClosedMonoidalCategory (NaturalTransformation c) (FTensor t) where
  apply = NT (apply . first (Proxy :: Proxy c) runET . lowerFTensor)

instance (ClosedMonoidalCategory c t, TOb ob t, ob (Unit c t), TOb ob (Exp c)) =>
         ClosedMonoidalCategory (FullSubcategory ob c) t where
  apply = FS apply

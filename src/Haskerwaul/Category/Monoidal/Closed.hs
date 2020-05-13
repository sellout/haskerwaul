{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Closed
  ( module Haskerwaul.Category.Monoidal.Closed
  -- * extended modules
  , module Haskerwaul.Category.Closed
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Constraint ((\\))
import qualified Data.Function as Base hiding ((.), id)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))
import qualified Data.Tuple as Base

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Closed
import Haskerwaul.Category.Kleisli
import Haskerwaul.Category.Monoidal
import Haskerwaul.Monad
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/closed+monoidal+category)
class (ClosedCategory c, MonoidalCategory c t) =>
      ClosedMonoidalCategory c t where
  apply :: (Ob c a, Ob c b) => t (Exp c a b) a `c` b

instance ClosedMonoidalCategory (->) (,) where
  apply = Base.uncurry (Base.$)

instance (d ~ (->), ClosedMonoidalCategory d t) =>
         ClosedMonoidalCategory (NaturalTransformation d) (FTensor t) where
  apply = NT (apply . first (Proxy :: Proxy d) runET . lowerFTensor)

instance {-# overlappable #-}
         ( c ~ (->)
         , ClosedMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m
         , BOb (Ob c) (Ob c) (Ob c) (Kleisli (Exp c) m)) =>
         ClosedMonoidalCategory (Kleisli (c :: Type -> Type -> Type) m) t where
  apply :: forall a b. (Ob c a, Ob c b) => Kleisli c m (t (Exp (Kleisli c m) a b) a) b
  apply =
    Kleisli (apply . first (Proxy :: Proxy c) runKleisli)
    \\ inF @(Ob c) @(Ob c) @m @b

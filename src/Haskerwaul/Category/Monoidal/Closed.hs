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
  apply :: (Ob c a, Ob c b) => t (InternalHom c a b) a `c` b
  curry :: (Ob c x, Ob c y, Ob c z) => t x y `c` z -> x `c` InternalHom c y z

-- | This forms at least an adjunction with curry -- maybe an isomorphism, even.
uncurry :: forall c t x y z. (ClosedMonoidalCategory c t, Ob c x, Ob c y, Ob c z)
        => x `c` InternalHom c y z -> t x y `c` z
uncurry f = apply . first (Proxy :: Proxy c) f \\ inT @(Ob c) @(InternalHom c) @y @z

instance ClosedMonoidalCategory (->) (,) where
  apply = Base.uncurry (Base.$)
  curry = Base.curry

instance (d ~ (->), ClosedMonoidalCategory d dt) =>
         ClosedMonoidalCategory (NaturalTransformation c d) (FTensor dt) where
  apply = NT (apply . first (Proxy :: Proxy d) runET . lowerFTensor)
  curry (NT f) = NT (ET . curry (f . FTensor))

instance {-# overlappable #-}
         ( c ~ (->)
         , ClosedMonoidalCategory c t
         , Bifunctor (Kleisli c m) (Kleisli c m) (Kleisli c m) t
         , Monad c m) =>
         ClosedMonoidalCategory (Kleisli (c :: Type -> Type -> Type) m) t where
  apply :: forall a b. (Ob c a, Ob c b) => Kleisli c m (t (InternalHom (Kleisli c m) a b) a) b
  apply =
    Kleisli (apply . first (Proxy :: Proxy c) runKleisli)
    \\ inF @(Ob c) @(Ob c) @m @b
  curry (Kleisli f) = Kleisli (pure . Kleisli . curry f)

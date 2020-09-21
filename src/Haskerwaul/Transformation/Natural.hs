{-# language TypeApplications #-}

module Haskerwaul.Transformation.Natural where

import qualified Control.Applicative as Base
import qualified Control.Category as Base
import qualified Control.Monad as Base
import           Data.Constraint ((:-))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Constraint
import Haskerwaul.Monoid
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/natural+transformation)
newtype NaturalTransformation c d f g =
  NT { runNT :: forall a. Ob c a => f a `d` g a }

type instance Ob (NaturalTransformation c d) = FOb (Ob c) (Ob d)

instance Ob c ~ All => MonoidalCategory' (NaturalTransformation c c) Compose where
  type Unit (NaturalTransformation c c) Compose = Identity

instance MonoidalCategory' (NaturalTransformation c (:-)) CFProd where
  type Unit (NaturalTransformation c (:-)) CFProd = All

-- | Tensor product for monoidal structures lifted from the destination
--   category.
newtype FTensor t f g a = FTensor { lowerFTensor :: (t (f a) (g a)) }

instance (Ob d ~ All, MonoidalCategory' d dt) =>
         MonoidalCategory' (NaturalTransformation c d) (FTensor dt) where
  type Unit (NaturalTransformation c d) (FTensor dt) = Const (Unit d dt)

-- `Haskerwaul.Monad.Monad` instances

instance {-# overlappable #-} Base.Monad f =>
                              Magma (NaturalTransformation (->) (->)) Compose f where
  op = NT (Base.join Base.. getCompose)

instance {-# overlappable #-} Base.Monad f =>
                              Semigroup (NaturalTransformation (->) (->)) Compose f

instance {-# overlappable #-} Base.Monad f =>
                              UnitalMagma (NaturalTransformation (->) (->)) Compose f where
  unit Proxy = NT (Base.pure Base.. runIdentity)

-- `Base.Alternative` instances

-- | This lowers a `Magma` in a functor category to one in the target category.
lowerOp
  :: forall c d dt f a
   . (d ~ (->), Magma (NaturalTransformation c d) (FTensor dt) f, Ob c a)
  => Proxy c -> dt (f a) (f a) `d` f a
lowerOp Proxy = runNT @_ @c op Base.. FTensor

-- | This lowers a `UnitalMagma` in a functor category to one in the target category.
lowerUnit
  :: forall c d dt f a
   . (d ~ (->), UnitalMagma (NaturalTransformation c d) (FTensor dt) f, Ob c a)
  => Proxy c -> Proxy dt -> Unit d dt `d` f a
lowerUnit Proxy Proxy = runNT @_ @c (unit (Proxy :: Proxy (FTensor dt))) Base.. Const

instance {-# overlappable #-} Base.Alternative f =>
                              Magma (NaturalTransformation (->) (->)) (FTensor (,)) f where
  op = NT (\(FTensor (a, b)) -> a Base.<|> b)

instance {-# overlappable #-} Base.Alternative f =>
                              Semigroup (NaturalTransformation (->) (->)) (FTensor (,)) f

instance {-# overlappable #-} Base.Alternative f =>
                              UnitalMagma (NaturalTransformation (->) (->)) (FTensor (,)) f where
  unit Proxy = NT (\(Const ()) -> Base.empty)

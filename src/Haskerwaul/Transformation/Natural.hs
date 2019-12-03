module Haskerwaul.Transformation.Natural where

import qualified Control.Applicative as Base
import qualified Control.Category as Base
import qualified Control.Monad as Base
import           Data.Constraint ((:-))
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Const (Const)
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Constraint
import Haskerwaul.Magma.Unital
import Haskerwaul.Object
import Haskerwaul.Semigroup

-- | https://ncatlab.org/nlab/show/natural+transformation
newtype NaturalTransformation c f g = NT { runNT :: forall a. f a `c` g a }

type instance Ob (NaturalTransformation _) = All

instance MonoidalCategory' (NaturalTransformation c) Compose where
  type Unit (NaturalTransformation c) Compose = Identity

instance MonoidalCategory' (NaturalTransformation (:-)) CFProd where
  type Unit (NaturalTransformation (:-)) CFProd = All

-- | Tensor product for monoidal structures lifted from the destination
--   category.
newtype FTensor t f g a = FTensor { lowerFTensor :: (t (f a) (g a)) }

instance MonoidalCategory' c t =>
         MonoidalCategory' (NaturalTransformation c) (FTensor t) where
  type Unit (NaturalTransformation c) (FTensor t) = Const (Unit c t)

-- * `Haskerwaul.Monad.Monad` instances

instance {-# overlappable #-} Base.Monad f =>
                              Magma (NaturalTransformation (->)) Compose f where
  op = NT (Base.join Base.. getCompose)

instance {-# overlappable #-} Base.Monad f =>
                              Semigroup (NaturalTransformation (->)) Compose f

instance {-# overlappable #-} Base.Monad f =>
                              UnitalMagma (NaturalTransformation (->)) Compose f where
  unit Proxy = NT (Base.pure Base.. runIdentity)

-- * __FIXME__: Get rid of everything below here in favor of using
--             `NaturalTransformation` with
--             `Haskerwaul.Category.Product.ProductCategory`.

newtype NaturalTransformation2 (c :: ok -> ok -> Type) (f :: j -> k -> ok) (g :: j -> k -> ok) =
  NT2 { runNT2 :: forall a b. f a b `c` g a b }

type instance Ob (NaturalTransformation2 _) = All

newtype BTensor t f g a b = BTensor { lowerBTensor :: (t (f a b) (g a b)) }

instance MonoidalCategory' c t =>
         MonoidalCategory' (NaturalTransformation2 c) (BTensor t) where
  type Unit (NaturalTransformation2 c) (BTensor t) = BConst (Unit c t)

newtype BConst a b c = BConst { getBConst :: a }

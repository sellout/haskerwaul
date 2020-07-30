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
newtype NaturalTransformation d f g = NT { runNT :: forall a. f a `d` g a }

-- | __FIXME__: This should maintain the @`Ob` c@ constraint somehow. E.g.,
--             @`FOb` (`Ob` c) (`Ob` d)@.
type instance Ob (NaturalTransformation d) = All

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

-- `Haskerwaul.Monad.Monad` instances

instance {-# overlappable #-} Base.Monad f =>
                              Magma (NaturalTransformation (->)) Compose f where
  op = NT (Base.join Base.. getCompose)

instance {-# overlappable #-} Base.Monad f =>
                              Semigroup (NaturalTransformation (->)) Compose f

instance {-# overlappable #-} Base.Monad f =>
                              UnitalMagma (NaturalTransformation (->)) Compose f where
  unit Proxy = NT (Base.pure Base.. runIdentity)

-- `Base.Alternative` instances

-- | This lowers a `Magma` in a functor category to one in the target category.
lowerOp
  :: (c ~ (->), Magma (NaturalTransformation c) (FTensor t) f)
  => t (f a) (f a) `c` f a
lowerOp = runNT op Base.. FTensor

-- | This lowers a `UnitalMagma` in a functor category to one in the target category.
lowerUnit
  :: forall c t f a
   . (c ~ (->), UnitalMagma (NaturalTransformation c) (FTensor t) f)
  => Proxy t -> Unit c t `c` f a
lowerUnit Proxy = runNT (unit (Proxy :: Proxy (FTensor t))) Base.. Const

instance {-# overlappable #-} Base.Alternative f =>
                              Magma (NaturalTransformation (->)) (FTensor (,)) f where
  op = NT (\(FTensor (a, b)) -> a Base.<|> b)

instance {-# overlappable #-} Base.Alternative f =>
                              Semigroup (NaturalTransformation (->)) (FTensor (,)) f

instance {-# overlappable #-} Base.Alternative f =>
                              UnitalMagma (NaturalTransformation (->)) (FTensor (,)) f where
  unit Proxy = NT (\(Const ()) -> Base.empty)

-- * __FIXME__: Get rid of everything below here in favor of using
--             `NaturalTransformation` with
--             `Haskerwaul.Category.Product.ProductCategory`.

-- | Like `NaturalTransformation`, but over a bifunctor.
newtype NaturalTransformation2 d f g =
  NT2 { runNT2 :: forall a b. f a b `d` g a b }

-- | __FIXME__: This should maintain the @`Ob` c@ constraint somehow. E.g.,
--             @`BOb` (`Ob` c1) (`Ob` c2) (`Ob` d)@.
type instance Ob (NaturalTransformation2 _) = All

-- | Like `FTensor`, but lifted from the target category to a bifunctor.
newtype BTensor t f g a b = BTensor { lowerBTensor :: (t (f a b) (g a b)) }

instance MonoidalCategory' c t =>
         MonoidalCategory' (NaturalTransformation2 c) (BTensor t) where
  type Unit (NaturalTransformation2 c) (BTensor t) = BConst (Unit c t)

-- | Like `Const`, but a bifunctor.
newtype BConst a b c = BConst { getBConst :: a }

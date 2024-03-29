{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Haskerwaul.Transformation.Natural where

import qualified Control.Applicative as Base
import qualified Control.Category as Base
import qualified Control.Monad as Base
import Data.Constraint (Dict (..), refl, (:-) (..), (\\))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality ((:~:) (..), type (~))
#else
import Data.Type.Equality ((:~:) (..))
#endif
import Haskerwaul.Category
import Haskerwaul.Constraint
import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/natural+transformation)
newtype NaturalTransformation (c :: ok -> ok -> Type) d f g = NT {runNT :: forall a. (Ob c a) => f a `d` g a}

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/whiskering)
whisker ::
  forall c d e f g h.
  ( e ~ (->),
    Functor d e h,
    Ob (NaturalTransformation c d) f,
    Ob (NaturalTransformation c d) g
  ) =>
  Proxy h ->
  NaturalTransformation c d f g ->
  NaturalTransformation c e (Compose h f) (Compose h g)
whisker Proxy (NT fn) =
  NT
    ( \(x :: Compose h f a) ->
        Compose (map fn (getCompose x))
          \\ inF @(Ob c) @(Ob d) @f @a
          \\ inF @(Ob c) @(Ob d) @g @a
    )

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/whiskering)
whisker' ::
  forall c d e f g h.
  (e ~ (->), Ob (NaturalTransformation c d) f) =>
  NaturalTransformation d e g h ->
  Proxy f ->
  NaturalTransformation c e (Compose g f) (Compose h f)
whisker' (NT fn) Proxy =
  NT
    ( \(x :: Compose g f a) ->
        Compose (fn (getCompose x))
          \\ inF @(Ob c) @(Ob d) @f @a
    )

type instance Ob (NaturalTransformation c d) = FOb (Ob c) (Ob d)

instance (Ob d ~ All) => MonoidalCategory' (NaturalTransformation c d) Compose where
  type Unit (NaturalTransformation c d) Compose = Identity

instance MonoidalCategory' (NaturalTransformation c (:-)) CFProd where
  type Unit (NaturalTransformation c (:-)) CFProd = All

-- | Tensor product for monoidal structures lifted from the destination
--   category.
--
-- - @`FTensor` (,) ~ `Data.Functor.Product.Product`@
-- - @`FTensor` `Data.Either.Either` ~ `Data.Functor.Sum.Sum`@
newtype FTensor t f g a = FTensor {lowerFTensor :: t (f a) (g a)}

instance
  (Ob d ~ All, MonoidalCategory' d dt) =>
  MonoidalCategory' (NaturalTransformation c d) (FTensor dt)
  where
  type Unit (NaturalTransformation c d) (FTensor dt) = Const (Unit d dt)

-- | If /D/ is a `Magmoid`, then so are /D/-valued functors.
instance
  (Magmoid d) =>
  Magma (DinaturalTransformation (->)) Procompose (NaturalTransformation c d)
  where
  op = DT (\(Procompose (NT f) (NT g)) -> NT (f . g))

-- | If /D/ is a `Semicategory`, then so are /D/-valued functors.
instance
  (FlexibleMagmoid d) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose (NaturalTransformation c d)

-- | If /D/ is a `Semicategory`, then so are /D/-valued functors.
instance
  (Semicategory d) =>
  Semigroup (DinaturalTransformation (->)) Procompose (NaturalTransformation c d)

-- | If /D/ is a `UnitalMagmoid`, then so are /D/-valued functors.
instance
  (UnitalMagmoid d) =>
  UnitalMagma (DinaturalTransformation (->)) Procompose (NaturalTransformation c d)
  where
  unit Proxy = DT (\Refl -> NT id)

-- `Haskerwaul.Monad.Monad` instances

instance
  {-# OVERLAPPABLE #-}
  (Base.Monad f) =>
  Magma (NaturalTransformation (->) (->)) Compose f
  where
  op = NT (Base.join Base.. getCompose)

instance
  {-# OVERLAPPABLE #-}
  (Base.Monad f) =>
  FlexibleMagma (NaturalTransformation (->) (->)) Compose f

instance
  {-# OVERLAPPABLE #-}
  (Base.Monad f) =>
  Semigroup (NaturalTransformation (->) (->)) Compose f

instance
  {-# OVERLAPPABLE #-}
  (Base.Monad f) =>
  UnitalMagma (NaturalTransformation (->) (->)) Compose f
  where
  unit Proxy = NT (Base.pure Base.. runIdentity)

-- `Base.Alternative` instances

-- | This lowers a `Magma` in a functor category to one in the target category.
lowerOp ::
  forall c d dt f a.
  (d ~ (->), Magma (NaturalTransformation c d) (FTensor dt) f, Ob c a) =>
  Proxy c ->
  dt (f a) (f a) `d` f a
lowerOp Proxy = runNT @_ @c op . FTensor

-- | This lowers a `UnitalMagma` in a functor category to one in the target category.
lowerUnit ::
  forall c d dt f a.
  (d ~ (->), UnitalMagma (NaturalTransformation c d) (FTensor dt) f, Ob c a) =>
  Proxy c ->
  Proxy dt ->
  Unit d dt `d` f a
lowerUnit Proxy Proxy = runNT @_ @c (unit (Proxy :: Proxy (FTensor dt))) . Const

instance
  {-# OVERLAPPABLE #-}
  (Base.Alternative f) =>
  Magma (NaturalTransformation (->) (->)) (FTensor (,)) f
  where
  op = NT (\(FTensor (a, b)) -> a Base.<|> b)

instance
  {-# OVERLAPPABLE #-}
  (Base.Alternative f) =>
  FlexibleMagma (NaturalTransformation (->) (->)) (FTensor (,)) f

instance
  {-# OVERLAPPABLE #-}
  (Base.Alternative f) =>
  Semigroup (NaturalTransformation (->) (->)) (FTensor (,)) f

instance
  {-# OVERLAPPABLE #-}
  (Base.Alternative f) =>
  UnitalMagma (NaturalTransformation (->) (->)) (FTensor (,)) f
  where
  unit Proxy = NT (\(Const ()) -> Base.empty)

instance Magma (NaturalTransformation c (:-)) CFProd All where
  op = NT (Sub Dict)

instance FlexibleMagma (NaturalTransformation c (:-)) CFProd All

instance Semigroup (NaturalTransformation c (:-)) CFProd All

instance UnitalMagma (NaturalTransformation c (:-)) CFProd All where
  unit Proxy = NT refl

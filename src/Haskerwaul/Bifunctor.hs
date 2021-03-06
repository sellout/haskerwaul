{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Bifunctor where

import qualified Data.Bifunctor as Base
import           Data.Constraint ((\\), (:-)(..), (:=>)(..), (***), Class(..), trans)
import           Data.Functor.Const (Const (..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/bifunctor)
--
--  __TODO__: This should be
--           @type Bifunctor c1 c2 = `Haskerwaul.Functor.Functor` (c1 :**: c2)@
class BOb (Ob c1) (Ob c2) (Ob d) f => Bifunctor c1 c2 d f where
  bimap :: (Ob c1 a1, Ob c1 b1, Ob c2 a2, Ob c2 b2)
        => a1 `c1` b1 -> a2 `c2` b2 -> f a1 a2 `d` f b1 b2

first :: forall c1 c2 d f a1 b1 a2.
         (Category c2, Bifunctor c1 c2 d f, Ob c1 a1, Ob c1 b1, Ob c2 a2)
      => Proxy c2 -> a1 `c1` b1 -> f a1 a2 `d` f b1 a2
first Proxy f = bimap @_ @c2 f id

second :: forall c1 c2 d f a1 a2 b2.
          (Category c1, Bifunctor c1 c2 d f, Ob c1 a1, Ob c2 a2, Ob c2 b2)
       => Proxy c1 -> a2 `c2` b2 -> f a1 a2 `d` f a1 b2
second Proxy g = bimap @c1 id g

instance Base.Bifunctor f => Bifunctor (->) (->) (->) f where
  bimap = Base.bimap

instance Bifunctor (->) c (->) Const where
  bimap f _ = Const . f . getConst

instance (d ~ (->), Semigroupoid d, Bifunctor c1 c2 d t) =>
         Bifunctor
         (NaturalTransformation c' c1)
         (NaturalTransformation c' c2)
         (NaturalTransformation c' d)
         (FTensor t) where
  bimap
    :: forall a1 a2 b1 b2
     . ( Ob (NaturalTransformation c' c1) a1
       , Ob (NaturalTransformation c' c1) b1
       , Ob (NaturalTransformation c' c2) a2
       , Ob (NaturalTransformation c' c2) b2)
    => NaturalTransformation c' c1 a1 b1
    -> NaturalTransformation c' c2 a2 b2
    -> NaturalTransformation c' d (FTensor t a1 a2) (FTensor t b1 b2)
  bimap (NT f) (NT g) =
    NT (\(x :: FTensor t a1 a2 x) ->
          FTensor (bimap f g (lowerFTensor x))
          \\ inF @(Ob c') @(Ob c1) @a1 @x
          \\ inF @(Ob c') @(Ob c1) @b1 @x
          \\ inF @(Ob c') @(Ob c2) @a2 @x
          \\ inF @(Ob c') @(Ob c2) @b2 @x)

instance (Ob c1 ~ All, Ob c2 ~ All, d ~ (->), Semigroupoid d, Bifunctor c1 c2 d t) =>
         Bifunctor
         (DinaturalTransformation c1)
         (DinaturalTransformation c2)
         (DinaturalTransformation d)
         (BTensor t) where
  bimap f g = DT (BTensor . bimap (runDT f) (runDT g) . lowerBTensor)

instance (c1 ~ (->), c2 ~ (->)) =>
         Bifunctor
         (DinaturalTransformation c1)
         (DinaturalTransformation c2)
         (DinaturalTransformation (->))
         Procompose where
  bimap f g = DT (\(Procompose x y) -> Procompose (runDT f x) (runDT g y))

instance Bifunctor (:-) (:-) (:-) Combine where
  bimap f g = trans ins (trans (f *** g) cls)

instance Bifunctor
         (NaturalTransformation c (:-))
         (NaturalTransformation c (:-))
         (NaturalTransformation c (:-))
         CFProd where
  bimap f g = NT (trans (trans ins (runNT f *** runNT g)) cls)

instance Bifunctor c1 c2 (->) (BConst a) where
  bimap _ _ (BConst a) = BConst a

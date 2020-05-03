{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Bifunctor where

import qualified Data.Bifunctor as Base
import           Data.Constraint ((:-)(..), (:=>)(..), (***), Class(..), trans)
import           Data.Functor.Const (Const (..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Object
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

instance (Semigroupoid c1, Bifunctor c1 c2 d t) =>
         Bifunctor (Opposite c1) (Opposite c2) (Opposite d) t where
  bimap f g = Opposite (bimap (opposite f) (opposite g))

instance (Ob c1 ~ All, Ob c2 ~ All, d ~ (->), Semigroupoid d, Bifunctor c1 c2 d t) =>
         Bifunctor
         (NaturalTransformation c1)
         (NaturalTransformation c2)
         (NaturalTransformation d)
         (FTensor t) where
  bimap f g = NT (FTensor . bimap (runNT f) (runNT g) . lowerFTensor)

instance (Ob c1 ~ All, Ob c2 ~ All, d ~ (->), Semigroupoid d, Bifunctor c1 c2 d t) =>
         Bifunctor
         (NaturalTransformation2 c1)
         (NaturalTransformation2 c2)
         (NaturalTransformation2 d)
         (BTensor t) where
  bimap f g = NT2 (BTensor . bimap (runNT2 f) (runNT2 g) . lowerBTensor)

instance (c1 ~ (->), c2 ~ (->)) =>
         Bifunctor
         (NaturalTransformation2 c1)
         (NaturalTransformation2 c2)
         (NaturalTransformation2 (->))
         CProd where
  bimap f g = NT2 (\(CProd x y) -> CProd (runNT2 f x) (runNT2 g y))

instance Bifunctor (:-) (:-) (:-) Combine where
  bimap f g = trans ins (trans (f *** g) cls)

instance Bifunctor
         (NaturalTransformation (:-))
         (NaturalTransformation (:-))
         (NaturalTransformation (:-))
         CFProd where
  bimap f g = NT (trans (trans ins (runNT f *** runNT g)) cls)

-- * `Haskerwaul.Profunctor.Profunctor` instances

-- | The arrow of every `Semigroupoid` is a `Haskerwaul.Profunctor.Profunctor`.
instance Semigroupoid c => Bifunctor (Opposite c) c (->) c where
  bimap f g fn = g . fn . opposite f

-- instance Bifunctor (Opposite (:-)) (:-) (:-) (:=>) where
--   -- bimap :: b :- a -> c :- d -> (a :=> c) :- (b :=> d)
--   bimap f g = trans g (trans ins (opposite f))

instance Bifunctor c1 c2 (->) (BConst a) where
  bimap _ _ (BConst a) = BConst a

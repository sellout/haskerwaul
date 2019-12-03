{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Bifunctor where

import qualified Data.Bifunctor as Base
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | https://ncatlab.org/nlab/show/bifunctor
--
--  __TODO__: This should be
--           @type Bifunctor c1 c2 = `Haskerwaul.Functor.Functor` (c1 :**: c2)@
class BOb (Ob i) (Ob j) (Ob k) f => Bifunctor i j k f where
  bimap :: (Ob i a, Ob i b, Ob j c, Ob j d)
        => a `i` b -> c `j` d -> f a c `k` f b d

first :: forall i j k f a b c.
         (Category j, Bifunctor i j k f, Ob i a, Ob i b, Ob j c)
      => Proxy j -> a `i` b -> f a c `k` f b c
first Proxy f = bimap @_ @j f id

second :: forall i j k f a c d.
          (Category i, Bifunctor i j k f, Ob i a, Ob j c, Ob j d)
       => Proxy i -> c `j` d -> f a c `k` f a d
second Proxy g = bimap @i id g

-- first :: (Bifunctor j k l f, Category k, Ob j a, Ob j b, Ob k c)
--       => Proxy k -> a `j` b -> f (a :**: c) `l` f (b :**: c)
-- first Proxy f = map (ProdC f id)

-- second :: (Bifunctor j k l f, Category j, Ob j a, Ob k b, Ob k c)
--        => Proxy j -> b `k` c -> f (a :**: b) `l` f (a :**: c)
-- second Proxy f = map (ProdC id f)

instance Base.Bifunctor f => Bifunctor (->) (->) (->) f where
  bimap = Base.bimap

instance (Semigroupoid c, Bifunctor c d e t) =>
         Bifunctor (Opposite c) (Opposite d) (Opposite e) t where
  bimap f g = Opposite (bimap (opposite f) (opposite g))

instance (Ob c ~ All, Ob d ~ All, e ~ (->), Semigroupoid e, Bifunctor c d e t) =>
         Bifunctor
         (NaturalTransformation c)
         (NaturalTransformation d)
         (NaturalTransformation e)
         (FTensor t) where
  bimap f g = NT (FTensor . bimap (runNT f) (runNT g) . lowerFTensor)

instance (Ob c ~ All, Ob d ~ All, e ~ (->), Semigroupoid e, Bifunctor c d e t) =>
         Bifunctor
         (NaturalTransformation2 c)
         (NaturalTransformation2 d)
         (NaturalTransformation2 e)
         (BTensor t) where
  bimap f g = NT2 (BTensor . bimap (runNT2 f) (runNT2 g) . lowerBTensor)

instance (c ~ (->), d ~ (->)) =>
         Bifunctor
         (NaturalTransformation2 c)
         (NaturalTransformation2 d)
         (NaturalTransformation2 (->))
         CProd where
  bimap f g = NT2 (\(CProd x y) -> CProd (runNT2 f x) (runNT2 g y))

-- instance Bifunctor (:-) (:-) (:-) (,) where

-- instance Bifunctor
--          (NaturalTransformation (:-))
--          (NaturalTransformation (:-))
--          (NaturalTransformation (:-))
--          CFProd where
--   -- bimap :: forall a b c d
--   --        . (forall i.              a i :- b i)
--   --       -> (forall i.              c i :- d i)
--   --          (forall i0 i1. (a i0, c i1) :- (b i0, d i1)) -- (runNT f *** runNT g)
--   --       -> (forall i.     CFProd a c i :- CFProd b d i)
--   bimap f g = NT (runNT f *** runNT g)

-- * `Haskerwaul.Profunctor.Profunctor` instances

-- | The arrow of every `Semigroupoid` is a `Haskerwaul.Profunctor.Profunctor`.
instance Semigroupoid c => Bifunctor (Opposite c) c (->) c where
  bimap f g fn = g . fn . opposite f

-- instance Bifunctor (Opposite (:-)) (:-) (:-) (:=>) where
--   bimap :: b :- a -> c :- d -> (a :=> c) :- (b :=> d)
--   bimap f g = trans g (trans fn (opposite f))

{-# language UndecidableInstances #-}

module Haskerwaul.Day where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Duoid.Components
import Haskerwaul.Functor.Monoidal.Lax
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/Day+convolution)
data Day c ct dt f g a = forall x y. Day (dt (f x) (g y)) (ct x y `c` a)

-- | Functor categories have `Day` as a monoidal tensor.
--
--  __NB__: This is overspecialized.
instance (c ~ (->), ct ~ (,), d ~ (->), dt ~ (,)) =>
         SemigroupalCategory
         (FullSubcategory (Functor c d) (NaturalTransformation d))
         (Day c ct dt) where
  assoc =
    Iso
    (FS (NT (\(Day (fa, Day (gb, hc) ifn) ofn) ->
                Day (Day (fa, gb) (\(a, b) -> \c -> ofn (a, ifn (b, c))), hc) (\(fn, c) -> fn c))))
    (FS (NT (\(Day (Day (fa, gb) ifn, hc) ofn) ->
                Day (fa, Day (gb, hc) (\(b, c) -> \a -> ofn (ifn (a, b), c))) (\(a, fn) -> fn a))))

instance MonoidalCategory' c ct =>
         MonoidalCategory' (NaturalTransformation d) (Day c ct dt) where
  type Unit (NaturalTransformation d) (Day c ct dt) = c (Unit c ct)

-- | Functor categories have `Day` as a monoidal tensor.
--
--  __NB__: Super over-specialized.
instance (c ~ (->), ct ~ (,), d ~ (->), dt ~ (,), MonoidalCategory d dt) =>
         MonoidalCategory
         (FullSubcategory (Functor c d) (NaturalTransformation d))
         (Day c ct dt) where
  leftIdentity =
    Iso
    (FS (NT (\(Day (fa, gb) fn) -> map (\b -> fn (fa (), b)) gb)))
    (FS (NT (\fa -> Day (\x -> x, fa) (\(_, a) -> a))))
  rightIdentity =
    Iso
    (FS (NT (\(Day (fa, gb) fn) -> map (\a -> fn (a, gb ())) fa)))
    (FS (NT (\fa -> Day (fa, \x -> x) (\(a, _) -> a))))

instance (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
         Magma (NaturalTransformation d) (Day c ct dt) f where
  op = NT (\(Day t fn) -> map fn (mu (Proxy :: Proxy c) t))

-- instance (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
--          UnitalMagma (NaturalTransformation d) (Day c ct dt) f where
--   unit :: Proxy (Day c ct dt)
--        -> NaturalTransformation d (Unit (NaturalTransformation d) (Day c ct dt)) f
--   unit Proxy = NT (epsilon (Proxy :: Proxy c) (Proxy :: Proxy ct) (Proxy :: Proxy dt))

instance (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
         Semigroup (NaturalTransformation d) (Day c ct dt) f

instance (d ~ (->), Semigroupoid c, Functor c d f, Functor c d g) =>
         Functor c d (Day c ct dt f g) where
  map f = \(Day t fn) -> Day t (f . fn)

instance (d ~ (->), Semigroupoid c) =>
         BOb (Functor c d) (Functor c d) (Functor c d) (Day c ct dt) where
  inB = Sub Dict

instance (d ~ (->), Category c, Bifunctor d d d dt) =>
         Bifunctor
         (FullSubcategory (Functor c d) (NaturalTransformation d))
         (FullSubcategory (Functor c d) (NaturalTransformation d))
         (FullSubcategory (Functor c d) (NaturalTransformation d))
         (Day c ct dt) where
  bimap f g =
    FS (NT (\(Day t fn) -> Day (bimap (runNT (inclusion f)) (runNT (inclusion g)) t) fn))

instance Semigroup (->) (,) a =>
         Magma (NaturalTransformation (->)) (Day (->) (,) (,)) (Diamond (Either a)) where
  op = NT (\(Day (Diamond fa, Diamond gb) fn) -> Diamond (case (fa, gb) of
              (Left a,  Left a')  -> Left (op (a, a'))
              (Left a,  Right _)  -> Left a
              (Right _, Left a)   -> Left a
              (Right b, Right b') -> Right (fn (b, b'))))

-- | The `Semigroup` constraint here is redundant, but we should only have a
--  `Star` instance when we have a `Diamond` instance.
instance Semigroup (->) (,) a =>
         Magma (NaturalTransformation (->)) (Day (->) (,) (,)) (Star (Either a)) where
  op = NT (\(Day (Star fa, Star gb) fn) -> Star (runNT op (Day (fa, gb) fn)))

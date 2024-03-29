{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Day where

import Data.Constraint (Dict (..), (:-) (..))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Constraint
import Haskerwaul.Duoid.Components
import Haskerwaul.Functor.Monoidal.Lax
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/Day+convolution)
data Day c ct dt f g a
  = forall x y. (Ob c x, Ob c y) => Day (dt (f x) (g y)) (ct x y `c` a)

-- | Functor categories have `Day` as a monoidal tensor.
--
--  __NB__: This is overspecialized.
instance
  (c ~ (->), ct ~ (,), d ~ (->), dt ~ (,)) =>
  SemigroupalCategory (NaturalTransformation c d) (Day c ct dt)
  where
  assoc =
    Iso
      ( NT
          ( \(Day (fa, Day (gb, hc) ifn) ofn) ->
              Day (Day (fa, gb) (\(a, b) c -> ofn (a, ifn (b, c))), hc) (\(fn, c) -> fn c)
          )
      )
      ( NT
          ( \(Day (Day (fa, gb) ifn, hc) ofn) ->
              Day (fa, Day (gb, hc) (\(b, c) a -> ofn (ifn (a, b), c))) (\(a, fn) -> fn a)
          )
      )

instance
  (MonoidalCategory' c ct, FOb (Ob c) (Ob d) (c (Unit c ct))) =>
  MonoidalCategory' (NaturalTransformation c d) (Day c ct dt)
  where
  type Unit (NaturalTransformation c d) (Day c ct dt) = c (Unit c ct)

-- | Functor categories have `Day` as a monoidal tensor.
--
--  __NB__: Super over-specialized.
instance
  (c ~ (->), ct ~ (,), d ~ (->), dt ~ (,), MonoidalCategory d dt) =>
  MonoidalCategory
    (FullSubcategory (Functor c d) (NaturalTransformation c d))
    (Day c ct dt)
  where
  leftIdentity =
    Iso
      (FS (NT (\(Day (fa, gb) fn) -> map (\b -> fn (fa (), b)) gb)))
      (FS (NT (\fa -> Day (id, fa) exr)))
  rightIdentity =
    Iso
      (FS (NT (\(Day (fa, gb) fn) -> map (\a -> fn (a, gb ())) fa)))
      (FS (NT (\fa -> Day (fa, id) exl)))

instance
  (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
  Magma (NaturalTransformation c d) (Day c ct dt) f
  where
  op = NT (\(Day t fn) -> map fn (mu (Proxy :: Proxy c) t))

instance
  (c ~ (->), ct ~ (,), d ~ (->), dt ~ (,), LaxMonoidalFunctor c ct d dt f) =>
  UnitalMagma (NaturalTransformation c d) (Day c ct dt) f
  where
  unit Proxy =
    NT (\fn -> map (const (fn ())) (epsilon (Proxy :: Proxy c) (Proxy :: Proxy ct) (Proxy :: Proxy dt) ()))

instance
  (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
  FlexibleMagma (NaturalTransformation c d) (Day c ct dt) f

instance
  (c ~ (->), d ~ (->), LaxMonoidalFunctor c ct d dt f) =>
  Semigroup (NaturalTransformation c d) (Day c ct dt) f

instance (d ~ (->), Semicategory c) => Semifunctor c d (Day c ct dt f g) where
  map f (Day t fn) = Day t (f . fn)

instance (d ~ (->), Category c) => Functor c d (Day c ct dt f g)

instance
  (d ~ (->), Semicategory c) =>
  BOb All All (Semifunctor c d) (Day c ct dt)
  where
  inB = Sub Dict

instance (d ~ (->), Category c) => BOb All All (Functor c d) (Day c ct dt) where
  inB = Sub Dict

instance
  (d ~ (->), Category c, Bifunctor d d d dt) =>
  Bifunctor
    (NaturalTransformation c d)
    (NaturalTransformation c d)
    (NaturalTransformation c d)
    (Day c ct dt)
  where
  bimap f g =
    NT (\(Day t fn) -> Day (bimap @d @d (runNT @_ @c f) (runNT @_ @c g) t) fn)

instance
  (Semigroup (->) (,) a) =>
  Magma (NaturalTransformation (->) (->)) (Day (->) (,) (,)) (Diamond (Either a))
  where
  op =
    NT
      ( \(Day (Diamond fa, Diamond gb) fn) ->
          Diamond
            ( case (fa, gb) of
                (Left a, Left a') -> Left (op (a, a'))
                (Left a, Right _) -> Left a
                (Right _, Left a) -> Left a
                (Right b, Right b') -> Right (fn (b, b'))
            )
      )

-- | The `Semigroup` constraint here is redundant, but we should only have a
--  `Star` instance when we have a `Diamond` instance.
instance
  (Semigroup (->) (,) a) =>
  Magma (NaturalTransformation (->) (->)) (Day (->) (,) (,)) (Star (Either a))
  where
  op = NT (\(Day (Star fa, Star gb) fn) -> Star (runNT @_ @(->) op (Day (fa, gb) fn)))

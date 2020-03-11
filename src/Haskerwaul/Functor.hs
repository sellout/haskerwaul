{-# language TypeApplications
           , UndecidableInstances #-}

-- | Haskerwaul also provides functors that aren't implementable via the type
--   class here. E.g., there is `Haskerwaul.Bifunctor.Bifunctor` and
--  `Haskerwaul.Profunctor.Profunctor`, but even beyond those are things like
--  `Haskerwaul.Subcategory.Full.inclusion`, which logically forms a faithful
--   functor from the subcategory, but has no type constructor to allow an
--   instance of `Functor`. Likewise, `Control.Arrow.Arrow` has a functor
--  `Control.Arrow.arr` from __Hask__ to the `Control.Arrow.Arrow` instance.
module Haskerwaul.Functor where

import           Data.Constraint ((:-)(..), Dict(..), mapDict)
import qualified Data.Functor as Base
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Const (Const(..))

import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | [nLab](https://ncatlab.org/nlab/show/functor)
class FOb (Ob c) (Ob d) f => Functor c d f where
  map :: (Ob c a, Ob c b) => a `c` b -> f a `d` f b

-- | The composition of two functors is always a functor.
--
--  __NB__: This constrains @f@ to be a __Hask__-valued functor, because
--         `Compose` is. It also constrains @b@ to @(->)@, but that is perhaps
--          fixable with something like defining our own `Compose` that carries
--          the "middle" category around.
instance (b ~ (->), c ~ (->), Semigroupoid c, Functor b c f, Functor a b g) =>
         Functor a c (Compose f g) where
  map f = Compose . map @b (map @_ @b f) . getCompose

-- | __NB__: This instance only exists to eliminate the ambiguity between the
--          `Base.Functor` constrained instance and the above instance when
--           trying to satisfy @`Functor` (->) (->) (`Compose` f g)@.
instance {-# overlapping #-} (Functor (->) (->) f, Functor (->) (->) g) =>
         Functor (->) (->) (Compose f g) where
  map f = Compose . map @(->) (map @_ @(->) f) . getCompose

-- | This encodes that a composition of functors is always a functor, and has a
--   similar restriction to the @`Functor` (`Compose` f g)@ instance.
instance (b ~ (->), c ~ (->), Semigroupoid c) =>
         BOb (Functor b c) (Functor a b) (Functor a c) Compose where
  inB = Sub Dict

-- | `Dict` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor` between
--   the category of constraints and __Hask__.
instance Functor (:-) (->) Dict where
  map = mapDict

-- | This instance lifts all instances of `Base.Functor` to _our_ `Functor`. If
--   you're trying to define an instance where the source and destination
--   categories are both @(->)@, you should be instantiating `Base.Functor`
--   directly, and allow this instance to lift it into Haskerwaul.
instance {-# overlappable #-} Base.Functor f => Functor (->) (->) f where
  map = Base.fmap

-- | The constant functor to a particular object in the target category.
instance (d ~ (->), FOb (Ob c) (Ob d) (Const dOb)) =>
         Functor c d (Const dOb) where
  map _ = Const . getConst

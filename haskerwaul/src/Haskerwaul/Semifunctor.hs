{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Haskerwaul also provides functors that aren't implementable via the type
--   class here. E.g., there is `Haskerwaul.Bifunctor.Bifunctor` and
--  `Haskerwaul.Profunctor.Profunctor`, but even beyond those are things like
--  `Haskerwaul.Subcategory.Full.inclusion`, which logically forms a faithful
--   functor from the subcategory, but has no type constructor to allow an
--   instance of `Functor`. Likewise, `Control.Arrow.Arrow` has a functor
--  `Control.Arrow.arr` from __Hask__ to the `Control.Arrow.Arrow` instance.
module Haskerwaul.Semifunctor where

import Data.Constraint (Dict (..), mapDict, (:-) (..))
import qualified Data.Functor as Base
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Object
import Haskerwaul.Semicategory

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/semifunctor)
class
  (Semicategory c, Semicategory d, FOb (Ob c) (Ob d) f) =>
  Semifunctor c d f
  where
  map :: (Ob c a, Ob c b) => a `c` b -> f a `d` f b

-- | The composition of two functors is always a functor.
--
--  __NB__: This constrains @f@ to be a __Hask__-valued functor, because
--         `Compose` is. It also constrains @b@ to @(->)@, but that is perhaps
--          fixable with something like defining our own `Compose` that carries
--          the "middle" category around.
instance
  (b ~ (->), c ~ (->), Semicategory a, Semifunctor b c f, Semifunctor a b g) =>
  Semifunctor a c (Compose f g)
  where
  map f = Compose . map @b (map @_ @b f) . getCompose

-- | __NB__: This instance only exists to eliminate the ambiguity between the
--          `Base.Functor` constrained instance and the above instance when
--           trying to satisfy @`Functor` (->) (->) (`Compose` f g)@.
instance
  {-# OVERLAPPING #-}
  (Semifunctor (->) (->) f, Semifunctor (->) (->) g) =>
  Semifunctor (->) (->) (Compose f g)
  where
  map f = Compose . map @(->) (map @_ @(->) f) . getCompose

-- | This encodes that a composition of functors is always a functor, and has a
--   similar restriction to the @`Functor` (`Compose` f g)@ instance.
instance
  (b ~ (->), c ~ (->), Semicategory c) =>
  BOb (Semifunctor b c) (Semifunctor a b) (Semifunctor a c) Compose
  where
  inB = Sub Dict

-- | `Dict` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor` between
--   the category of constraints and __Hask__.
instance Semifunctor (:-) (->) Dict where
  map = mapDict

-- | This instance lifts all instances of `Base.Functor` to _our_ `Functor`. If
--   you're trying to define an instance where the source and destination
--   categories are both @(->)@, you should be instantiating `Base.Functor`
--   directly, and allow this instance to lift it into Haskerwaul.
--
--  __NB__: This instance can be @INCOHERENT@ with an instance like
--          @`Semifunctor` c (->) Foo@, in which case we prefer the other
--          instance.
instance {-# INCOHERENT #-} (Base.Functor f) => Semifunctor (->) (->) f where
  map = Base.fmap

-- | The constant functor to a particular object in the target category.
instance
  (d ~ (->), Semicategory c, FOb (Ob c) (Ob d) (Const dOb)) =>
  Semifunctor c d (Const dOb)
  where
  map _ = Const . getConst

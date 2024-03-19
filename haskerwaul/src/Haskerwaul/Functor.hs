{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
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
module Haskerwaul.Functor
  ( module Haskerwaul.Functor,

    -- * extended modules
    module Haskerwaul.Semifunctor,
  )
where

import Data.Constraint (Dict (..), (:-) (..))
import qualified Data.Functor as Base
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Category
import Haskerwaul.Object
import Haskerwaul.Semifunctor

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/functor)
class (Category c, Semifunctor c d f) => Functor c d f

-- | The composition of two functors is always a functor.
--
--  __NB__: This constrains @f@ to be a __Hask__-valued functor, because
--         `Compose` is. It also constrains @b@ to @(->)@, but that is perhaps
--          fixable with something like defining our own `Compose` that carries
--          the "middle" category around.
instance
  (b ~ (->), c ~ (->), Semicategory c, Functor b c f, Functor a b g) =>
  Functor a c (Compose f g)

-- | __NB__: This instance only exists to eliminate the ambiguity between the
--          `Base.Functor` constrained instance and the above instance when
--           trying to satisfy @`Functor` (->) (->) (`Compose` f g)@.
instance
  {-# OVERLAPPING #-}
  (Functor (->) (->) f, Functor (->) (->) g) =>
  Functor (->) (->) (Compose f g)

-- | This encodes that a composition of functors is always a functor, and has a
--   similar restriction to the @`Functor` (`Compose` f g)@ instance.
instance
  (b ~ (->), c ~ (->), Semicategory c) =>
  BOb (Functor b c) (Functor a b) (Functor a c) Compose
  where
  inB = Sub Dict

-- | `Dict` is a `Haskerwaul.Functor.Faithful.Full.FullFaithfulFunctor` between
--   the category of constraints and __Hask__.
instance Functor (:-) (->) Dict

-- | This instance lifts all instances of `Base.Functor` to _our_ `Functor`. If
--   you're trying to define an instance where the source and destination
--   categories are both @(->)@, you should be instantiating `Base.Functor`
--   directly, and allow this instance to lift it into Haskerwaul.
--
--  __NB__: This instance can be @INCOHERENT@ with an instance like @`Functor` c
--          (->) Foo@, in which case we prefer the other instance.
instance {-# INCOHERENT #-} (Base.Functor f) => Functor (->) (->) f

-- | The constant functor to a particular object in the target category.
instance
  (d ~ (->), Category c, FOb (Ob c) (Ob d) (Const dOb)) =>
  Functor c d (Const dOb)

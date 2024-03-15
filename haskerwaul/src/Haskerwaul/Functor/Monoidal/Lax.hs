{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Functor.Monoidal.Lax
  ( module Haskerwaul.Functor.Monoidal.Lax,

    -- * extended modules
    module Haskerwaul.Functor,
  )
where

import qualified Control.Applicative as Base
import Data.Constraint ((\\))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import qualified Data.Void as Base
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Endofunctor
import Haskerwaul.Functor
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+functor)
class
  (MonoidalCategory c ct, MonoidalCategory d dt, Functor c d f) =>
  LaxMonoidalFunctor c ct d dt f
  where
  epsilon :: Proxy c -> Proxy ct -> Proxy dt -> Unit d dt `d` f (Unit c ct)
  mu :: (Ob c x, Ob c y) => Proxy c -> dt (f x) (f y) `d` f (ct x y)

-- | `Applicative` is base's view of a `LaxMonoidalFunctor` on the Cartesian
--   product in __Hask__.
instance (Base.Applicative f) => LaxMonoidalFunctor (->) (,) (->) (,) f where
  epsilon Proxy Proxy Proxy = Base.pure
  mu Proxy = uncurry (Base.liftA2 (,))

-- | `LaxMonoidalFunctors` on `Either` in __Hask__ are trivial.
instance (Endofunctor (->) f) => LaxMonoidalFunctor (->) Either (->) Either f where
  epsilon Proxy Proxy Proxy = Base.absurd
  mu Proxy = \case
    Left fx -> map Left fx
    Right fy -> map Right fy

-- | A `LaxMonoidalFunctor`s entire purpose is to preserve the monoidal
--   structure from the source (@a@) to the destination (@f a@).
mapOp ::
  forall c ct d dt f a.
  (LaxMonoidalFunctor c ct d dt f, Magma c ct a) =>
  Proxy c ->
  Proxy ct ->
  dt (f a) (f a) `d` (f a)
mapOp Proxy Proxy =
  map (op @c @ct) . mu (Proxy :: Proxy c) \\ inT @(Ob c) @ct @a @a

-- | A `LaxMonoidalFunctor`s entire purpose is to preserve the monoidal
--   structure from the source (@a@) to the destination (@f a@).
mapUnit ::
  forall c ct d dt f a.
  (LaxMonoidalFunctor c ct d dt f, UnitalMagma c ct a) =>
  Proxy c ->
  Proxy ct ->
  Proxy dt ->
  Unit d dt `d` (f a)
mapUnit c ct dt = map @c (unit ct) . epsilon c ct dt

{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal
  ( module Haskerwaul.Category.Duoidal
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Day
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/duoidal+category)
class (MonoidalCategory c di, MonoidalCategory c st) =>
      DuoidalCategory c di st where
  switch :: (Ob c w, Ob c x, Ob c y, Ob c z)
         => di (st w x) (st y z) `c` st (di w y) (di x z)
  duplicateUnit :: Proxy di -> Unit c di `c` st (Unit c di) (Unit c di)
  combineUnit :: Proxy st -> di (Unit c st) (Unit c st) `c` Unit c st

foreUnit :: forall c di st
          . ( Ob c (di (Unit c st) (Unit c di)),
              Ob c (di (Unit c di) (Unit c st)),
              Ob c (st (Unit c st) (Unit c di)),
              Ob c (st (Unit c di) (Unit c st)),
              DuoidalCategory c di st)
         => Proxy di -> Proxy st -> Unit c di `c` Unit c st
foreUnit Proxy Proxy =
  to (leftIdentity @c @st)
  . bimap (to (rightIdentity @c @di)) (to (leftIdentity @c @di))
  . switch
  . bimap (from (leftIdentity @c @st)) (from (rightIdentity @c @st))
  . from (rightIdentity @c @di)

-- | `Endofunctor` categories are normal duoidal with respect to `Day` and
--   `Compose`.
--
--  __NB__: This is currently specialized to `Endofunctor`s over __Hask__.
--          Should be able to generalize this.
instance (c ~ (->), ct ~ (,)) =>
         DuoidalCategory
         (FullSubcategory (Endofunctor c) (NaturalTransformation c))
         (Day c ct ct)
         Compose where
  switch =
    FS (NT (\(Day t fn) -> Compose (Day (bimap getCompose getCompose t) (\t' -> Day t' fn))))
  duplicateUnit Proxy = FS (NT (\const -> Compose (\_ -> const)))
  combineUnit Proxy =
    FS (NT (\(Day t fn) -> Identity (fn (bimap runIdentity runIdentity t))))

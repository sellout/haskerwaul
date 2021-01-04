{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal
  ( module Haskerwaul.Category.Duoidal
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Functor.Compose (Compose(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Comonoid
import Haskerwaul.Day
import Haskerwaul.Endofunctor
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/duoidal+category)
--
--   The other operations (@I → I ⋆ I@, @J ⋄ J → J@, and @I → J@) are implied by
--   the `Monoid` and `Comonoid` constraints.
class (MonoidalCategory c di, MonoidalCategory c st, Monoid c di (Unit c st), Comonoid c st (Unit c di)) =>
      DuoidalCategory c di st where
  interchange :: (Ob c w, Ob c x, Ob c y, Ob c z)
              => di (st w x) (st y z) `c` st (di w y) (di x z)

-- | `Endofunctor` categories are normal duoidal with respect to `Day` and
--   `Compose`.
--
--  __NB__: This is currently specialized to `Endofunctor`s over __Hask__.
--          Should be able to generalize this.
instance (c ~ (->), ct ~ (,)) =>
         DuoidalCategory
         (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
         (Day c ct ct)
         Compose where
  interchange =
    FS (NT (\(Day t fn) ->
              Compose (Day (bimap getCompose getCompose t) (\t' -> Day t' fn))))

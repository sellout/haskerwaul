{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal.Normal
  ( module Haskerwaul.Category.Duoidal.Normal,

    -- * extended modules
    module Haskerwaul.Category.Duoidal,
  )
where

import Data.Constraint ((\\))
import Data.Functor.Compose (Compose)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Bimonoid
import Haskerwaul.Category.Duoidal
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Day
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

class (DuoidalCategory c di st) => NormalDuoidalCategory c di st where
  -- | This must form a lawful isomorphism with `unit`.
  counit :: Proxy di -> Proxy st -> Unit c st `c` Unit c di

unitIso ::
  forall c di st.
  (NormalDuoidalCategory c di st) =>
  Proxy di ->
  Proxy st ->
  Isomorphism c (Unit c di) (Unit c st)
unitIso di st =
  Iso (unit di) (counit di st)
    \\ inT @(Ob c) @di @(Unit c st) @(Unit c di)
    \\ inT @(Ob c) @di @(Unit c di) @(Unit c st)
    \\ inT @(Ob c) @st @(Unit c st) @(Unit c di)
    \\ inT @(Ob c) @st @(Unit c di) @(Unit c st)

instance
  {-# OVERLAPPABLE #-}
  (BraidedMonoidalCategory c t, Bimonoid c t (Unit c t)) =>
  NormalDuoidalCategory c t t
  where
  counit Proxy Proxy = id

-- | `Endofunctor` categories are normal duoidal with respect to `Day` and
--   `Compose`.
--
--  __NB__: This is currently specialized to `Endofunctor`s over __Hask__.
--          Should be able to generalize this.
instance
  (c ~ (->), ct ~ (,)) =>
  NormalDuoidalCategory
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    (Day c ct ct)
    Compose
  where
  counit Proxy Proxy = FS (NT (\u () -> runIdentity u))

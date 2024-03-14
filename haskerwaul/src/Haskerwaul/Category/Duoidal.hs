{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal
  ( module Haskerwaul.Category.Duoidal,

    -- * extended modules
    module Haskerwaul.Category.Monoidal,
  )
where

import Data.Constraint ((\\))
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Bimonoid
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Day
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/duoidal+category)
--
--   The other operations (@I → I ⋆ I@, @J ⋄ J → J@, and @I → J@) are implied by
--   the `Monoid` and `Comonoid` constraints.
class
  ( MonoidalCategory c di,
    MonoidalCategory c st,
    -- LaxMonoidalFunctor c di c di (Diagonal st),
    -- LaxMonoidalFunctor c di c di (Unit c st),
    Monoid c di (Unit c st),
    Comonoid c st (Unit c di)
  ) =>
  DuoidalCategory (c :: k -> k -> Type) (di :: k -> k -> k) (st :: k -> k -> k)
  where
  interchange ::
    (Ob c w, Ob c x, Ob c y, Ob c z) =>
    di (st w x) (st y z) `c` st (di w y) (di x z)

-- interchange = unDiagonal . mu Proxy . Diagonal

instance {-# OVERLAPPABLE #-} (BraidedMonoidalCategory c t, Bimonoid c t (Unit c t)) => DuoidalCategory c t t where
  interchange ::
    forall w x y z.
    (Ob c w, Ob c x, Ob c y, Ob c z) =>
    t (t w x) (t y z) `c` t (t w y) (t x z)
  interchange =
    to assoc
      . second (Proxy @c) (from assoc . first @_ @_ @c (Proxy @c) (to (braid @c)) . to assoc)
      . from assoc
      \\ inT @(Ob c) @t @x @(t y z)
      \\ inT @(Ob c) @t @y @(t x z)
      \\ inT @(Ob c) @t @x @y
      \\ inT @(Ob c) @t @x @z
      \\ inT @(Ob c) @t @y @x
      \\ inT @(Ob c) @t @y @z

-- | `Endofunctor` categories are normal duoidal with respect to `Day` and
--   `Compose`.
--
--  __NB__: This is currently specialized to `Endofunctor`s over __Hask__.
--          Should be able to generalize this.
instance
  (c ~ (->), ct ~ (,)) =>
  DuoidalCategory
    (FullSubcategory (Endofunctor c) (NaturalTransformation c c))
    (Day c ct ct)
    Compose
  where
  interchange =
    FS
      ( NT
          ( \(Day t fn) ->
              Compose (Day (bimap getCompose getCompose t) (`Day` fn))
          )
      )

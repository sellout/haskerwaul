{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Compact
  ( module Haskerwaul.Category.Closed.Compact,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Closed,
    module Haskerwaul.Category.Monoidal.Traced,
  )
where

import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Rigid
import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Category.Monoidal.Traced
import Haskerwaul.Object
import Haskerwaul.Object.Invertible

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/compact+closed+category)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Compact_closed_category)
--
--  __NB__: Instances for this are autmatically coalesced.
class
  ( ClosedMonoidalCategory c t,
    RigidMonoidalCategory c t,
    SymmetricMonoidalCategory c t,
    forall a. (Ob c a) => Invertible c t a
  ) =>
  CompactClosedCategory c t

instance
  ( ClosedMonoidalCategory c t,
    RigidMonoidalCategory c t,
    SymmetricMonoidalCategory c t,
    forall a. (Ob c a) => Invertible c t a
  ) =>
  CompactClosedCategory c t

-- -- | Compact closed categories are self-dual.
-- instance CompactClosedCategory c t => CompactClosedCategory (Opposite c) t

-- -- | "Every compact closed category is equipped with a canonical trace [...]"
-- --   ⸻[nLab](https://ncatlab.org/nlab/show/traced+monoidal+category#in_cartesian_monoidal_categoeries)
-- instance {-# overlappable #-} CompactClosedCategory c t => TracedMonoidalCategory c t where
--   trace :: forall a b x. (Ob c a, Ob c b, Ob c x) => t a x `c` t b x -> a `c` b
--   trace f =
--     to rightIdentity
--     . bimap @c @c id leftUnit
--     . from assoc
--     . bimap @c @c f id
--     . to assoc
--     . bimap @c @c id rightUnit
--     . from rightIdentity
--     \\ inT @(Ob c) @t @x @(Dual c t x)
--     \\ inT @(Ob c) @t @a @x
--     \\ inT @(Ob c) @t @b @x

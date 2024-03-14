-- | This module provides the newtypes that are used to distinguish the
--   components of duoid-like structures. It also contains instances over these
--   types for classes defined in "base".
--
--  __NB__: Unlike the other @Components@ modules, these are defined over
--          functors rather than proper types. That's because the common cases
--          of duoids are functors, and so it's more useful to define them with
--          this kind, until we manage to make them kind-polymorphic.
module Haskerwaul.Duoid.Components where

newtype Diamond a i = Diamond { getDiamond :: a i }

newtype Star a i = Star { getStar :: a i }

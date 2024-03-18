{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- __NB__: We allow an orphan here, because of the cyclic definitions between
--         Cartesian `trace` and `fix`.
{-# OPTIONS_GHC -Wno-orphans #-}

module Haskerwaul.Category.Monoidal.Cartesian.Traced
  ( module Haskerwaul.Category.Monoidal.Cartesian.Traced,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Cartesian,
    module Haskerwaul.Category.Monoidal.Traced,
  )
where

import Control.Arrow ((&&&))
import Data.Constraint ((\\))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Traced
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/traced+monoidal+category#in_cartesian_monoidal_categories)
class
  (CartesianMonoidalCategory c, TracedMonoidalCategory c (Prod c)) =>
  TracedCartesianMonoidalCategory c
  where
  fix :: forall a x. (Ob c a, Ob c x) => Prod c a x `c` x -> a `c` x
  fix f = trace (bimap f f . diagonal) \\ inT @(Ob c) @(Prod c) @a @x

-- | A default implementation of `trace` for a `TracedCartesianMonoidalCategory`
--   if you'd rather define `fix` manually.
cartesianTrace ::
  forall c a b x.
  (TracedCartesianMonoidalCategory c, Ob c a, Ob c b, Ob c x) =>
  Prod c a x `c` Prod c b x ->
  a `c` b
cartesianTrace f = exl . f . (bimap @c id (fix (exr . f)) . diagonal)

instance TracedMonoidalCategory (->) (,) where
  trace = cartesianTrace

instance TracedCartesianMonoidalCategory (->) where
  fix f = f . (id &&& fix f)

{-# LANGUAGE Safe #-}

module Haskerwaul.Order.Canonical where

-- | Some type classes are too broad to pin down with a single instance. E.g.,
--   for `Haskerwaul.Preorder.Preorder`, even a type as simple as
--  `Data.Bool.Bool` has four different valid instances:
--
-- - @`Haskerwaul.Category.Closed.Cartesian.const` `Data.Bool.True`@
-- - @`Haskerwaul.Relation.Equality.eq`@
-- - @`Haskerwaul.Relation.Equality.Decidable.neq` (`Data.Bool.True`, `Data.Bool.False`)@
-- - @`Haskerwaul.Relation.Equality.Decidable.neq` (`Data.Bool.False`, `Data.Bool.True`)@
--
--   So `Canonical` is a way of tagging an instance that's implied as a
--   canonical ordering, as opposed to an equivalence relation or something.
newtype Canonical a = Canonical {decanonicalize :: a}

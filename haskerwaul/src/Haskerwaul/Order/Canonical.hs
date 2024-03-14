module Haskerwaul.Order.Canonical where

-- | Some type classes are too broad to pin down with a single instance. E.g., for `Preorder`, even
--   a type as simple as `Bool` has four different valid instances:
-- - @`const` `True`@
-- - @`eq`@
-- - @/= (True, False)@
-- - @/= (False, True)@
--
--   So `Canonical` is a way of tagging an instance that's implied as a canonical ordering, as
--   opposed to an equivalence relation or something.
newtype Canonical a = Canonical {decanonicalize :: a}

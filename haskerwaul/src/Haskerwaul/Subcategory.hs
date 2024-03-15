{-# LANGUAGE Safe #-}

module Haskerwaul.Subcategory where

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/subcategory)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Subcategory)
class Subcategory c d where
  -- | This is a faithful functor, but it doesn't fit the more restrictive type
  --   signature of `Haskerwaul.Functor.Faithful` (using `Identity` restricts
  --   the kind of the category).
  inclusion :: a `c` b -> a `d` b

-- instance (Subcategory c d, Subcategory d e) => Subcategory c e where
--   inclusion = inclusion . inclusion

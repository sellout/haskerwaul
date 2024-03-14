{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Traced
  ( module Haskerwaul.Category.Monoidal.Traced,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Balanced,
  )
where

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal.Balanced
import Haskerwaul.Object

-- |
-- = laws
--   [superposing]:
--     @`trace` (`to` `assoc` . (`id` *** f)) == `id` *** `trace` f@
--   [vanishing]: @`trace` (`from` `assoc` f) == `trace` (`trace` f) @
--   [yanking]: @`trace` `braid` == `id`@
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/traced+monoidal+category)
-- - [primary source](https://pdfs.semanticscholar.org/c232/37a187d026b8130d98c09187b8ba4f611c40.pdf)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Traced_monoidal_category)
class (BalancedMonoidalCategory c t) => TracedMonoidalCategory c t where
  -- |
  -- * references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/trace)
  trace :: (Ob c a, Ob c b, Ob c x) => t a x `c` t b x -> a `c` b

{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Boolean
  ( module Haskerwaul.Category.Boolean,

    -- * extended modules
    module Haskerwaul.Category.Heyting,
  )
where

import Haskerwaul.Category.Heyting

-- | Categories where
--   [excluded middle](http://ncatlab.org/nlab/show/excluded+middle) holds.
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/Boolean+category)
class (HeytingCategory c) => BooleanCategory c

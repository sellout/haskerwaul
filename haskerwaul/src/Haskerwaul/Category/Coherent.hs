{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Coherent
  ( module Haskerwaul.Category.Coherent,

    -- * extended modules
    module Haskerwaul.Category.Regular,
  )
where

import Haskerwaul.Category.Regular

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/coherent+category)
class (RegularCategory c) => CoherentCategory c

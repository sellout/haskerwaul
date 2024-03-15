{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Band.Rectangular
  ( module Haskerwaul.Band.Rectangular,

    -- * extended modules
    module Haskerwaul.Band,
  )
where

import Haskerwaul.Band

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/rectangular%20band)
class (Band c t a) => RectangularBand c t a

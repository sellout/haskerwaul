{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Graphic
  ( module Haskerwaul.Monoid.Graphic,

    -- * extended modules
    module Haskerwaul.Band.LeftRegular,
    module Haskerwaul.Monoid,
    module Haskerwaul.Shelf.Left,
  )
where

import Haskerwaul.Band.LeftRegular
import Haskerwaul.Monoid
import Haskerwaul.Shelf.Left

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/graphic+monoid)

---  __NB__: Instances for this are automatically coalesced.
class
  (LeftRegularBand c t a, LeftShelf c t a, Monoid c t a) =>
  GraphicMonoid c t a

instance
  (LeftRegularBand c t a, LeftShelf c t a, Monoid c t a) =>
  GraphicMonoid c t a

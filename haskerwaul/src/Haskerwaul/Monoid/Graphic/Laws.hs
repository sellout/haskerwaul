module Haskerwaul.Monoid.Graphic.Laws where

import Haskerwaul.Band.LeftRegular.Laws
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Monoid.Graphic
import Haskerwaul.Monoid.Laws
import Haskerwaul.Shelf.Left.Laws

data GraphicMonoidLaws c a =
  GraphicMonoidLaws
    { leftRegularBand :: LeftRegularBandLaws c a
    , leftShelf :: LeftShelfLaws c a
    , monoid :: MonoidLaws c (Prod c) a
    }

graphicMonoidLaws ::
  (CartesianMonoidalCategory c, GraphicMonoid c (Prod c) a) =>
  GraphicMonoidLaws c a
graphicMonoidLaws =
  GraphicMonoidLaws
    { leftRegularBand = leftRegularBandLaws
    , leftShelf = leftShelfLaws
    , monoid = monoidLaws
    }

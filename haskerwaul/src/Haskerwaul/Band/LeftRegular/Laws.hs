{-# LANGUAGE Safe #-}

module Haskerwaul.Band.LeftRegular.Laws where

import Haskerwaul.Band.Laws
import Haskerwaul.Band.LeftRegular
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Law
import Haskerwaul.Law.Identity.Graphic
import Haskerwaul.Relation.Equality

data LeftRegularBandLaws c a = LeftRegularBandLaws
  { band :: BandLaws c a,
    graphicIdentity :: Law c EqualityRelation (Prod c a a) a
  }

leftRegularBandLaws ::
  (CartesianMonoidalCategory c, LeftRegularBand c (Prod c) a) => LeftRegularBandLaws c a
leftRegularBandLaws =
  LeftRegularBandLaws
    { band = bandLaws,
      graphicIdentity = graphicIdentityLaw op
    }

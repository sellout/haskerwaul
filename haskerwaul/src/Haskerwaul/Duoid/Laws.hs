{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Duoid.Laws where

import Data.Proxy (Proxy (..))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Duoidal
import Haskerwaul.Category.Opposite
import Haskerwaul.Duoid
import Haskerwaul.Law
import Haskerwaul.Law.Interchange
import Haskerwaul.Monoid.Laws
import Haskerwaul.Relation.Equality

diamondUnitLaw :: forall c di st a. (Duoid c di st a) => Proxy di -> Proxy st -> Law c EqualityRelation (Unit c di) a
diamondUnitLaw di Proxy = Law (unit di) (op @_ @st . bimap (unit @c di) (unit @c di) . opposite op)

starUnitLaw :: forall c di st a. (Duoid c di st a) => Proxy st -> Law c EqualityRelation (di (Unit c st) (Unit c st)) a
starUnitLaw st = Law (unit st . op) (op . bimap (unit @c st) (unit @c st))

unitLaw :: (Duoid c di st a) => Proxy di -> Proxy st -> Law c EqualityRelation (Unit c di) a
unitLaw di st = Law (unit di) (unit st . unit di)

data DuoidLaws c di st a = DuoidLaws
  { diamond :: MonoidLaws c di a,
    star :: MonoidLaws c st a,
    interchange' :: Law c EqualityRelation (di (st a a) (st a a)) a,
    diamondUnit :: Law c EqualityRelation (Unit c di) a,
    starUnit :: Law c EqualityRelation (di (Unit c st) (Unit c st)) a,
    unit' :: Law c EqualityRelation (Unit c di) a
  }

duoidLaws :: forall c di st a. (Duoid c di st a) => DuoidLaws c di st a
duoidLaws =
  DuoidLaws
    { diamond = monoidLaws,
      star = monoidLaws,
      interchange' = interchangeLaw interchange,
      diamondUnit = diamondUnitLaw di st,
      starUnit = starUnitLaw st,
      unit' = unitLaw di st
    }
  where
    di = Proxy :: Proxy di
    st = Proxy :: Proxy st

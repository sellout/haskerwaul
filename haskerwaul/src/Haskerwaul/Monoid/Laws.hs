{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}

module Haskerwaul.Monoid.Laws where

import Haskerwaul.Category.Monoidal
import Haskerwaul.Magma.Unital.Laws
import Haskerwaul.Semigroup.Laws

data MonoidLaws c t a = MonoidLaws
  { semigroup :: SemigroupLaws c t a,
    unitalMagma :: UnitalMagmaLaws c t a
  }

-- | The constraints here should be simply @`Monoid` c t a@, the
--  `MonoidalCategory` should be included in the `Monoid` (but that last bit
--   causes a cycle currently).
monoidLaws :: (MonoidalCategory c t, Monoid c t a) => MonoidLaws c t a
monoidLaws =
  MonoidLaws
    { semigroup = semigroupLaws,
      unitalMagma = unitalMagmaLaws
    }

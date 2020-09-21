{-# language RecordWildCards
           , TypeApplications #-}

-- | This is the entry point for testing all of the classes provided by
-- Haskerwaul. Users probably shouldn't be using this directly, but this
-- provides a common structure for libraries like
-- [Hedgehog](https://hedgehog.qa/) to have testing libraries built on.
--
-- > -- This looks pretty good, but hardcoding the `===` sets the category we're in.
-- > semigroup_laws getT genA = do
-- >   let law = semigroupLaws (Hedgehog.===)
-- >   input <- forAll . join $ genT <$> join (genT <$> genA <*> genA) <*> genA
-- >   associative law input
--
--   Then you should be able to test that by doing something like 
module Haskerwaul.Semigroup.Laws where

import Haskerwaul.Law
import Haskerwaul.Law.Associativity
import Haskerwaul.Relation.Equality
import Haskerwaul.Topos.Elementary

data SemigroupLaws c t a =
  SemigroupLaws
    { associative :: Law c EqualityRelation (t (t a a) a) a
    }

semigroupLaws
  :: (SemigroupalCategory c t, Semigroup c t a) => SemigroupLaws c t a
semigroupLaws =
  SemigroupLaws
    { associative = associativity op
    }

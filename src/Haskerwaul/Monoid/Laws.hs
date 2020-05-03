{-# language RecordWildCards #-}

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
module Haskerwaul.Monoid.Laws where

import Haskerwaul.Magma.Unital.Laws
import Haskerwaul.Object
import Haskerwaul.Semigroup.Laws
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

data MonoidLaws c t a =
  MonoidLaws
    { semigroup :: SemigroupLaws c t a
    , unitalMagma :: UnitalMagmaLaws c t a
    }

-- | The constraints here should be simply @(`ElementaryTopos` c, `Monoid` c t
--   a)@, the `MonoidalCategory` should be included in the `Monoid` (but that
--   last bit causes a cycle currently). And I have no idea why it's failing to
--   resolve all the `Ob`s correctly.
monoidLaws :: ( Ob c (t (t a a) a), Ob c (t a a), Ob c (t (Unit c t) a), Ob c (t a (Unit c t))
              , ElementaryTopos c
              , MonoidalCategory c t, Monoid c t a)
           => BinaryRelation c a a -> MonoidLaws c t a
monoidLaws eq =
  -- __TODO__: Why can't I define this with @do@ notation?
  MonoidLaws
    { semigroup = semigroupLaws eq
    , unitalMagma = unitalMagmaLaws eq
    }

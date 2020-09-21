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
module Haskerwaul.Magma.Unital.Laws where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Monoidal
import Haskerwaul.Law
import Haskerwaul.Law.Identity.Left
import Haskerwaul.Law.Identity.Right
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

data UnitalMagmaLaws c t a =
  UnitalMagmaLaws
    { leftIdentity :: Law c EqualityRelation (t (Unit c t) a) a
    , rightIdentity :: Law c EqualityRelation (t a (Unit c t)) a
    }

unitalMagmaLaws
  :: forall c t a. (MonoidalCategory c t, UnitalMagma c t a) => UnitalMagmaLaws c t a
unitalMagmaLaws =
  UnitalMagmaLaws
    { leftIdentity = leftIdentityLaw op (unit p)
    , rightIdentity = rightIdentityLaw op (unit p)
    }
  \\ inT @(Ob c) @t @(Unit c t) @a
  \\ inT @(Ob c) @t @a @(Unit c t)
  where
    p = Proxy :: Proxy t

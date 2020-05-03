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
module Haskerwaul.Magma.Unital.Laws where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Law
import Haskerwaul.Law.Identity.Left
import Haskerwaul.Law.Identity.Right
import Haskerwaul.Object
import Haskerwaul.Relation.Binary
import Haskerwaul.Topos.Elementary

data UnitalMagmaLaws c t a =
  UnitalMagmaLaws
    { leftIdentity :: (t (Unit c t) a) `c` Class c
    , rightIdentity :: (t a (Unit c t)) `c` Class c
    }

unitalMagmaLaws :: forall c t a
                 . ( Ob c (t (Unit c t) a), Ob c (t a (Unit c t))
                   , ElementaryTopos c
                   , MonoidalCategory c t, UnitalMagma c t a)
                => BinaryRelation c a a -> UnitalMagmaLaws c t a
unitalMagmaLaws eq =
  UnitalMagmaLaws
    { leftIdentity = checkLaw (leftIdentityLaw op (unit p)) eq
    , rightIdentity = checkLaw (rightIdentityLaw op (unit p)) eq
    }
  where
    p = Proxy :: Proxy t

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Magma.Unital.Laws where

import Data.Constraint ((\\))
import Data.Proxy (Proxy (..))
import Haskerwaul.Category.Monoidal hiding (leftIdentity, rightIdentity)
import Haskerwaul.Law
import Haskerwaul.Law.Identity.Left
import Haskerwaul.Law.Identity.Right
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

data UnitalMagmaLaws c t a = UnitalMagmaLaws
  { leftIdentity :: Law c EqualityRelation (t (Unit c t) a) a,
    rightIdentity :: Law c EqualityRelation (t a (Unit c t)) a
  }

unitalMagmaLaws ::
  forall c t a. (MonoidalCategory c t, UnitalMagma c t a) => UnitalMagmaLaws c t a
unitalMagmaLaws =
  UnitalMagmaLaws
    { leftIdentity = leftIdentityLaw op (unit p),
      rightIdentity = rightIdentityLaw op (unit p)
    }
    \\ inT @(Ob c) @t @(Unit c t) @a
    \\ inT @(Ob c) @t @a @(Unit c t)
  where
    p = Proxy :: Proxy t

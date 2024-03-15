{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Law.Identity.Left where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

leftIdentityLaw ::
  forall c t a.
  (MonoidalCategory c t, Ob c a) =>
  t a a `c` a ->
  Unit c t `c` a ->
  Law c EqualityRelation (t (Unit c t) a) a
leftIdentityLaw op' unit' = Law (to leftIdentity) (op' . bimap @_ @c unit' id)

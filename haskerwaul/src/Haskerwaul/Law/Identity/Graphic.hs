{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Law.Identity.Graphic where

import Data.Constraint ((\\))
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/graphic+category#definition)
graphicIdentityLaw ::
  forall c a.
  (CartesianMonoidalCategory c, Ob c a) =>
  Prod c a a `c` a ->
  Law c EqualityRelation (Prod c a a) a
graphicIdentityLaw op' =
  Law op' (op' . bimap @c @c id (op' . to braid) . from assoc . bimap @c @c diagonal id)
    \\ inT @(Ob c) @(Prod c) @a @a

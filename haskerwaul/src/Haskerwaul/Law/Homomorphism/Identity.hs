-- __NB__: The `Ob` constraints look redundant, because they‘re not yet
--         specified on `.`..
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Haskerwaul.Law.Homomorphism.Identity where

import Data.Proxy (Proxy (..))
import Haskerwaul.Category.Monoidal
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality

-- | Test unital properties of a morphism. This is required (in addition to
--  `Haskerwaul.Law.Homomorphism.Magma.magmaHomomorphism`) if we want to ensure
--   that the morphism preserves identities.
identityHomomorphism ::
  (MonoidalCategory c t, Ob c a, Ob c b) =>
  Proxy t ->
  Unit c t `c` a ->
  Unit c t `c` b ->
  a `c` b ->
  Law c EqualityRelation (Unit c t) b
identityHomomorphism Proxy unitA unitB f = Law unitB (f . unitA)

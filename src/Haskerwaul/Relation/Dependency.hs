{-# language UndecidableSuperClasses #-}

module Haskerwaul.Relation.Dependency where

import Haskerwaul.Relation.Tolerance

-- | [Wikipedia](https://en.wikipedia.org/wiki/Dependency_relation)
--
-- = laws
--   [finite]: ?
--   [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`tolerate` x x = `Haskerwaul.Topos.Elementary.true`@
--   [`Haskerwaul.Law.Symmetry.symmetry`]: @`tolerate` x y ==> `tolerate` y x@
class ToleranceRelation c a => DependencyRelation c a

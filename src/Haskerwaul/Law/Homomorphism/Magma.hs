module Haskerwaul.Law.Homomorphism.Magma where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Law
import Haskerwaul.Object

-- | This homomorphism covers most properties for a single morphism (except
--   identities). E.g., this is also used for semigroup homomorphism. That is,
--   if we know @a@ is commutative, then this property holding guarantees that
--   @b@ is commutative in the image of @a@.
--
--  __NB__: This should also be able to test the functor laws. If we specialize
--        * @c ~ `Haskerwaul.Transformation.Natural.NaturalTransformation2` (->)@
--          and
--        * @t ~ `CProd`@,
--
--          leaving @a@ and @b@ to be morphisms in the source and target
--          categories, respectively.
magmaHomomorphism :: (SemigroupalCategory c t, Ob c a, Ob c b)
                  => t a a `c` a -> t b b `c` b -> a `c` b -> Law c (t a a) b
magmaHomomorphism opA opB f = Law (f . opA) (opB . bimap f f)

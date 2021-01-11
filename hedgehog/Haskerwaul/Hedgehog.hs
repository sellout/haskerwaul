{-# language OverloadedStrings
           , TypeApplications
           , UndecidableInstances #-}
{-# options_ghc -Wwarn #-}

-- | This wraps the laws from Haskerwaul in a way that makes them testable with
--   Hedgehog.
--
--   The equality relation for Hedgehog requires `Show` instances, but if an
--   input type isn't used as a result type in a test, then we accept an
--   explicit display function for the input type instead of relying on `show`.
--
--   Even though some laws may have multiple @translate@-style parameters, they
--   should generally be the same term for a particular category.
--
-- = Structure of @_laws@ definitions
--
-- 1. a label to include on any properties
-- 2. the underlying @Laws@ definition to use
-- 3. one homomorphism to `HH` for each property "shape",
-- 4. `Eq` and `Show` instances for each property result type,
-- 5. one display function for each input-only type (otherwise, they use `show` implicitly), and
-- 6. one generator for each input type (regardless of whether it's an output type).
module Haskerwaul.Hedgehog where

import Control.Monad ((=<<))
import Data.Constraint.Deferrable ((:~:)(..))
import Data.Eq (Eq)
import Data.Proxy (Proxy (..))
import Data.String (String, fromString)
import Data.Typeable (Typeable, typeRep)
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Laws
import Haskerwaul.Category.Semigroupal.Laws as Semigroupal
import Haskerwaul.Hedgehog.Topos
import Haskerwaul.Isomorphism.Laws
import Haskerwaul.Law
import Haskerwaul.Magma.Commutative.Laws
import Haskerwaul.Magma.Unital.Laws
import Haskerwaul.Monoid.Commutative.Laws as CommutativeMonoid
import Haskerwaul.Monoid.Laws
import Haskerwaul.Object
import Haskerwaul.Profunctor
import Haskerwaul.Rig.Laws as Rig
import Haskerwaul.Semigroup.Laws as Semigroup
import Haskerwaul.Semigroupoid.Laws
import Haskerwaul.Semiring
import Haskerwaul.Topos.Elementary hiding (leftIdentity, rightIdentity)
import Haskerwaul.Transformation.Dinatural
import Hedgehog hiding (label)
import Text.Show (Show (..))

showType :: forall a. Typeable a => Proxy a -> PropertyName
showType = fromString . show . typeRep

semigroup_laws
  :: forall c t a x y. (Typeable c, Eq y, Show y)
  => PropertyName
  -> SemigroupLaws c t a
  -> (t (t a a) a `c` a -> x `HH` y)
  -> (x -> String)
  -> Gen x
  -> [(PropertyName, Property)]
semigroup_laws label law translate display genX =
  [( "associative (" <> label <> ")"
   , property (runHH (checkLaw translate (Semigroup.associative law)) =<< forAllWith display genX))
  ]

commutativeMagma_laws
  :: forall c t a x y. (Typeable c, Eq y, Show y)
  => PropertyName
  -> CommutativeMagmaLaws c t a
  -> (t a a `c` a -> x `HH` y)
  -> (x -> String)
  -> Gen x
  -> [(PropertyName, Property)]
commutativeMagma_laws label law translate display genX =
  [( "commutative (" <> label <> ")"
   , property (runHH (checkLaw translate (commutative law)) =<< forAllWith display genX))
  ]

unitalMagma_laws
  :: (Eq y, Show y)
  => PropertyName
  -> UnitalMagmaLaws c t a
  -> (t (Unit c t) a `c` a -> x1 `HH` y)
  -> (t a (Unit c t) `c` a -> x2 `HH` y)
  -> (x1 -> String)
  -> (x2 -> String)
  -> Gen x1
  -> Gen x2
  -> [(PropertyName, Property)]
unitalMagma_laws label law transL transR dispL dispR genX1 genX2 =
  [ ( "left identity (" <> label <> ")"
    , property (runHH (checkLaw transL (leftIdentity law)) =<< forAllWith dispL $ genX1))
  , ( "right identity (" <> label <> ")"
    , property (runHH (checkLaw transR (rightIdentity law)) =<< forAllWith dispR $ genX2))
  ]

monoid_laws
  :: (Typeable c, Eq y, Show y)
  => PropertyName
  -> MonoidLaws c t a
  -> (t (t a a) a `c` a -> x `HH` y)
  -> (t (Unit c t) a `c` a -> x1 `HH` y)
  -> (t a (Unit c t) `c` a -> x2 `HH` y)
  -> (x -> String)
  -> (x1 -> String)
  -> (x2 -> String)
  -> Gen x
  -> Gen x1
  -> Gen x2
  -> [(PropertyName, Property)]
monoid_laws label law trans transL transR display dispL dispR genX genX1 genX2 =
  semigroup_laws label (semigroup law) trans display genX
  <> unitalMagma_laws label (unitalMagma law) transL transR dispL dispR genX1 genX2

commutativeMonoid_laws
  :: (Typeable c, Eq y, Show y)
  => PropertyName
  -> CommutativeMonoidLaws c t a
  -> (t a a `c` a -> x `HH` y)
  -> (t (t a a) a `c` a -> x0 `HH` y)
  -> (t (Unit c t) a `c` a -> x1 `HH` y)
  -> (t a (Unit c t) `c` a -> x2 `HH` y)
  -> (x -> String)
  -> (x0 -> String)
  -> (x1 -> String)
  -> (x2 -> String)
  -> Gen x
  -> Gen x0
  -> Gen x1
  -> Gen x2
  -> [(PropertyName, Property)]
commutativeMonoid_laws label law trans transA transL transR display dispA dispL dispR genX genX0 genX1 genX2 =
  commutativeMagma_laws label (commutativeMagma law) trans display genX
  <> monoid_laws label (CommutativeMonoid.monoid law) transA transL transR dispA dispL dispR genX0 genX1 genX2

-- |
--  __TODO__: Missing a lot of intermediate types here.
rig_laws 
  :: forall c t a x x0 x1 x2 y
   . (c ~ (->), BraidedMonoidalCategory c t, Typeable c, Eq y, Show y)
  => PropertyName
  -> RigLaws c t a
  -> (t a a `c` a -> x `HH` y)
  -> (t (t a a) a `c` a -> x0 `HH` y)
  -> (t (Unit c t) a `c` a -> x1 `HH` y)
  -> (t a (Unit c t) `c` a -> x2 `HH` y)
  -> (x -> String)
  -> (x0 -> String)
  -> (x1 -> String)
  -> (x2 -> String)
  -> Gen x
  -> Gen x0
  -> Gen x1
  -> Gen x2
  -> [(PropertyName, Property)]
rig_laws label law trans transA transL transR display dispA dispL dispR genX genX0 genX1 genX2 =
  commutativeMonoid_laws
    label
    (commutativeMonoid law)
    (trans . promap @c @c (bimap Add Add) sum)
    (transA . promap @c @c (bimap @c @c @c (bimap Add Add) Add) sum)
    (transL . promap @c @c (bimap @c id Add) sum)
    (transR . promap @c @c (bimap @_ @c Add id) sum)
    display dispA dispL dispR
    genX genX0 genX1 genX2
  <> monoid_laws
     label
     (Rig.monoid law)
     (transA . promap @c @c (bimap @c @c @c (bimap Multiply Multiply) Multiply) product)
     (transL . promap @c @c (bimap @c id Multiply) product)
     (transR . promap @c @c (bimap @_ @c Multiply id) product)
     dispA dispL dispR
     genX0 genX1 genX2

semigroupoid_laws
  :: forall ok (c :: ok -> ok -> *) (a :: ok) (b :: ok) y
   . (Typeable ok, Eq y, Show y)
  => PropertyName
  -> SemigroupoidLaws c
  -> (a `c` b -> y)
  -> (Procompose (Procompose c c) c a b -> String)
  -> Gen (Procompose (Procompose c c) c a b)
  -> [(PropertyName, Property)]
semigroupoid_laws label law post = semigroup_laws label law (HH . (post .) . runDT)

category_laws
  :: forall ok (c :: ok -> ok -> *) (a :: ok) (b :: ok) y
   . (Typeable ok, Eq y, Show y)
  => PropertyName
  -> CategoryLaws c
  -> (a `c` b -> y)
  -> (Procompose (Procompose c c) c a b -> String)
  -> (Procompose (:~:) c a b -> String)
  -> (Procompose c (:~:) a b -> String)
  -> Gen (Procompose (Procompose c c) c a b)
  -> Gen (Procompose (:~:) c a b)
  -> Gen (Procompose c (:~:) a b)
  -> [(PropertyName, Property)]
category_laws label law post =
  monoid_laws label law (HH . (post .) . runDT) (HH . (post .) . runDT) (HH . (post .) . runDT)

semigroupalCategory_laws
  :: forall ok (c :: ok -> ok -> *) (t :: ok -> ok -> ok) (a :: ok) (b :: ok) m n o x3 x4 y
   . (Typeable ok, Ob c m, Ob c n, Ob c o, Eq y, Show y, Eq x3, Show x3, Eq x4, Show x4)
  => PropertyName
  -> SemigroupalCategoryLaws c t
  -> (t m (t n o) `c` t m (t n o) -> x3 `HH` x3)
  -> (t (t m n) o `c` t (t m n) o -> x4 `HH` x4)
  -> (a `c` b -> y)
  -> (Procompose (Procompose c c) c a b -> String)
  -> (Procompose (:~:) c a b -> String)
  -> (Procompose c (:~:) a b -> String)
  -> Gen (Procompose (Procompose c c) c a b)
  -> Gen (Procompose (:~:) c a b)
  -> Gen (Procompose c (:~:) a b)
  -> Gen x3
  -> Gen x4
  -> [(PropertyName, Property)]
semigroupalCategory_laws label law trans1 trans2 post display dispL dispR genX genX1 genX2 genX3 genX4 =
  category_laws (label <> " category") (Semigroupal.category law) post display dispL dispR genX genX1 genX2
  <> isomorphism_laws (label <> " associative tensor") (Semigroupal.associative law) trans1 trans2 genX3 genX4

isomorphism_laws
  :: (Eq x1, Show x1, Eq x2, Show x2)
  => PropertyName
  -> IsomorphismLaws c a b
  -> (a `c` a -> x1 `HH` x1)
  -> (b `c` b -> x2 `HH` x2)
  -> Gen x1
  -> Gen x2
  -> [(PropertyName, Property)]
isomorphism_laws label law trans1 trans2 genX1 genX2 =
  [ ( "forward isomorphism (" <> label <> ")"
    , property (runHH (checkLaw trans1 (forward law)) =<< forAll genX1))
  , ( "backward isomorphism (" <> label <> ")"
    , property (runHH (checkLaw trans2 (backward law)) =<< forAll genX2))
  ]

-- Would love to have a function like
-- forall a b y. (Eq y, Show y) => a `c` b -> 

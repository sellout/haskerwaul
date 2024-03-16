{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

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

import safe Control.Monad ((=<<))
import safe Data.Eq (Eq)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (..))
import safe Data.String (String, fromString)
#if MIN_VERSION_base(4, 17, 0)
import safe Data.Type.Equality ((:~:) (..), type (~))
#else
import safe Data.Type.Equality ((:~:) (..))
#endif
import safe Data.Typeable (Typeable, typeRep)
import safe Haskerwaul.Band.Laws as Band
import safe Haskerwaul.Band.LeftRegular.Laws as LeftRegularBand
import safe Haskerwaul.Bifunctor
import safe Haskerwaul.Category.Laws
import safe Haskerwaul.Category.Semigroupal.Laws as Semigroupal
import safe Haskerwaul.Duoid.Laws
import Haskerwaul.Hedgehog.Topos
import safe Haskerwaul.Isomorphism.Laws
import safe Haskerwaul.Lattice
import safe Haskerwaul.Lattice.Bounded.Laws as BoundedLattice
import safe Haskerwaul.Lattice.Laws as Lattice
import safe Haskerwaul.Law
import safe Haskerwaul.Magma.Commutative.Laws
import safe Haskerwaul.Magma.Unital.Laws
import safe Haskerwaul.Monoid.Commutative.Laws as CommutativeMonoid
import safe Haskerwaul.Monoid.Graphic.Laws as GraphicMonoid
import safe Haskerwaul.Monoid.Laws as Monoid
import safe Haskerwaul.Object
import safe Haskerwaul.Profunctor
import safe Haskerwaul.Rig.Laws as Rig
import safe Haskerwaul.Semigroup.Commutative.Laws as CommutativeSemigroup
import safe Haskerwaul.Semigroup.Laws as Semigroup
import safe Haskerwaul.Semigroupoid.Laws
import safe Haskerwaul.Semilattice.Bounded.Laws as BoundedSemilattice
import safe Haskerwaul.Semilattice.Laws as Semilattice
import safe Haskerwaul.Semiring
import safe Haskerwaul.Shelf.Left.Laws
import safe Haskerwaul.Shelf.Right.Laws
import safe Haskerwaul.Topos.Elementary hiding (leftIdentity, rightIdentity)
import safe Haskerwaul.Transformation.Dinatural
import Hedgehog hiding (label)
import safe Text.Show (Show (..))

showType :: forall a. (Typeable a) => Proxy a -> PropertyName
showType = fromString . show . typeRep

semigroup_laws ::
  forall c t a x y.
  (Eq y, Show y) =>
  PropertyName ->
  SemigroupLaws c t a ->
  (t (t a a) a `c` a -> x `HH` y) ->
  (x -> String) ->
  Gen x ->
  [(PropertyName, Property)]
semigroup_laws label law translate display genX =
  [ ( "associative (" <> label <> ")",
      property (runHH (checkLaw translate (Semigroup.associative law)) =<< forAllWith display genX)
    )
  ]

commutativeMagma_laws ::
  forall c t a x y.
  (Eq y, Show y) =>
  PropertyName ->
  CommutativeMagmaLaws c t a ->
  (t a a `c` a -> x `HH` y) ->
  (x -> String) ->
  Gen x ->
  [(PropertyName, Property)]
commutativeMagma_laws label law translate display genX =
  [ ( "commutative (" <> label <> ")",
      property (runHH (checkLaw translate (commutative law)) =<< forAllWith display genX)
    )
  ]

unitalMagma_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  UnitalMagmaLaws c t a ->
  (t (Unit c t) a `c` a -> x1 `HH` y) ->
  (t a (Unit c t) `c` a -> x2 `HH` y) ->
  (x1 -> String) ->
  (x2 -> String) ->
  Gen x1 ->
  Gen x2 ->
  [(PropertyName, Property)]
unitalMagma_laws label law transL transR dispL dispR genX1 genX2 =
  [ ( "left identity (" <> label <> ")",
      property (runHH (checkLaw transL (leftIdentity law)) =<< forAllWith dispL $ genX1)
    ),
    ( "right identity (" <> label <> ")",
      property (runHH (checkLaw transR (rightIdentity law)) =<< forAllWith dispR $ genX2)
    )
  ]

band_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  BandLaws c a ->
  (Prod c (Prod c a a) a `c` a -> x `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (x -> String) ->
  Gen x ->
  Gen y ->
  [(PropertyName, Property)]
band_laws label law transA transI dispA genX genY =
  semigroup_laws label (Band.semigroup law) transA dispA genX
    <> [ ( "idempotency (" <> label <> ")",
           property (runHH (checkLaw transI (idempotent law)) =<< forAll genY)
         )
       ]

monoid_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  MonoidLaws c t a ->
  (t (t a a) a `c` a -> x `HH` y) ->
  (t (Unit c t) a `c` a -> x1 `HH` y) ->
  (t a (Unit c t) `c` a -> x2 `HH` y) ->
  (x -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  Gen x ->
  Gen x1 ->
  Gen x2 ->
  [(PropertyName, Property)]
monoid_laws label law trans transL transR display dispL dispR genX genX1 genX2 =
  semigroup_laws label (Monoid.semigroup law) trans display genX
    <> unitalMagma_laws label (unitalMagma law) transL transR dispL dispR genX1 genX2

commutativeSemigroup_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  CommutativeSemigroupLaws c t a ->
  (t a a `c` a -> x `HH` y) ->
  (t (t a a) a `c` a -> x0 `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  Gen x ->
  Gen x0 ->
  [(PropertyName, Property)]
commutativeSemigroup_laws label law trans transA display dispA genX genX0 =
  commutativeMagma_laws label (commutativeMagma law) trans display genX
    <> semigroup_laws label (CommutativeSemigroup.semigroup law) transA dispA genX0

commutativeMonoid_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  CommutativeMonoidLaws c t a ->
  (t a a `c` a -> x `HH` y) ->
  (t (t a a) a `c` a -> x0 `HH` y) ->
  (t (Unit c t) a `c` a -> x1 `HH` y) ->
  (t a (Unit c t) `c` a -> x2 `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen x1 ->
  Gen x2 ->
  [(PropertyName, Property)]
commutativeMonoid_laws label law trans transA transL transR display dispA dispL dispR genX genX0 genX1 genX2 =
  commutativeSemigroup_laws label (CommutativeMonoid.commutativeSemigroup law) trans transA display dispA genX genX0
    <> monoid_laws label (CommutativeMonoid.monoid law) transA transL transR dispA dispL dispR genX0 genX1 genX2

leftRegularBand_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  LeftRegularBandLaws c a ->
  (Prod c (Prod c a a) a `c` a -> x `HH` y) ->
  (Prod c a a `c` a -> x0 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen y ->
  [(PropertyName, Property)]
leftRegularBand_laws label law transA transG transI dispA dispG genX genX0 genY =
  band_laws label (LeftRegularBand.band law) transA transI dispA genX genY
    <> [ ( "graphic identity (" <> label <> ")",
           property (runHH (checkLaw transG (graphicIdentity law)) =<< forAllWith dispG $ genX0)
         )
       ]

leftShelf_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  LeftShelfLaws c a ->
  (Prod c a (Prod c a a) `c` a -> x `HH` y) ->
  (x -> String) ->
  Gen x ->
  [(PropertyName, Property)]
leftShelf_laws label law trans display gen =
  [ ( "left self-distributive (" <> label <> ")",
      property (runHH (checkLaw trans (leftSelfDistributive law)) =<< forAllWith display $ gen)
    )
  ]

rightShelf_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  RightShelfLaws c a ->
  (Prod c (Prod c a a) a `c` a -> x `HH` y) ->
  (x -> String) ->
  Gen x ->
  [(PropertyName, Property)]
rightShelf_laws label law trans display gen =
  [ ( "right self-distributive (" <> label <> ")",
      property (runHH (checkLaw trans (rightSelfDistributive law)) =<< forAllWith display $ gen)
    )
  ]

graphicMonoid_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  GraphicMonoidLaws c a ->
  (Prod c (Prod c a a) a `c` a -> x `HH` y) ->
  (Prod c a a `c` a -> x0 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (Prod c a (Prod c a a) `c` a -> xx `HH` y) ->
  (Prod c (Unit c (Prod c)) a `c` a -> x1 `HH` y) ->
  (Prod c a (Unit c (Prod c)) `c` a -> x2 `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  (xx -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen xx ->
  Gen x1 ->
  Gen x2 ->
  Gen y ->
  [(PropertyName, Property)]
graphicMonoid_laws label law transA transG transI trans transL transR dispA dispG disp dispL dispR genX genX0 genXX genX1 genX2 genY =
  leftRegularBand_laws label (leftRegularBand law) transA transG transI dispA dispG genX genX0 genY
    <> leftShelf_laws label (leftShelf law) trans disp genXX
    <> monoid_laws label (GraphicMonoid.monoid law) transA transL transR dispA dispL dispR genX genX1 genX2

semilattice_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  SemilatticeLaws c a ->
  (Prod c a a `c` a -> x `HH` y) ->
  (Prod c (Prod c a a) a `c` a -> x0 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen y ->
  [(PropertyName, Property)]
semilattice_laws label law trans transA transY display dispA genX genX0 genY =
  commutativeSemigroup_laws label (Semilattice.commutativeSemigroup law) trans transA display dispA genX genX0
    <> band_laws label (Semilattice.band law) transA transY dispA genX0 genY

boundedSemilattice_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  BoundedSemilatticeLaws c a ->
  (Prod c a a `c` a -> x `HH` y) ->
  (Prod c (Prod c a a) a `c` a -> x0 `HH` y) ->
  (Prod c (Unit c (Prod c)) a `c` a -> x1 `HH` y) ->
  (Prod c a (Unit c (Prod c)) `c` a -> x2 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (Prod c a (Prod c a a) `c` a -> xx `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  (xx -> String) ->
  Gen x ->
  Gen x0 ->
  Gen x1 ->
  Gen x2 ->
  Gen xx ->
  Gen y ->
  [(PropertyName, Property)]
boundedSemilattice_laws label law trans transA transL transR transI transG display dispA dispL dispR dispG genX genX0 genX1 genX2 genXX genY =
  commutativeMonoid_laws label (BoundedSemilattice.commutativeMonoid law) trans transA transL transR display dispA dispL dispR genX genX0 genX1 genX2
    <> graphicMonoid_laws label (graphicMonoid law) transA trans transI transG transL transR dispA display dispG dispL dispR genX0 genX genXX genX1 genX2 genY
    <> semilattice_laws label (semilattice law) trans transA transI display dispA genX genX0 genY

duoid_laws ::
  (Eq y, Show y) =>
  PropertyName ->
  DuoidLaws c di st a ->
  (di (di a a) a `c` a -> x `HH` y) ->
  (di (Unit c di) a `c` a -> x1 `HH` y) ->
  (di a (Unit c di) `c` a -> x2 `HH` y) ->
  (st (st a a) a `c` a -> x' `HH` y) ->
  (st (Unit c st) a `c` a -> x1' `HH` y) ->
  (st a (Unit c st) `c` a -> x2' `HH` y) ->
  (di (st a a) (st a a) `c` a -> x'' `HH` y) ->
  (Unit c di `c` a -> x1'' `HH` y) ->
  (di (Unit c st) (Unit c st) `c` a -> x2'' `HH` y) ->
  (x -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  (x' -> String) ->
  (x1' -> String) ->
  (x2' -> String) ->
  (x'' -> String) ->
  (x1'' -> String) ->
  (x2'' -> String) ->
  Gen x ->
  Gen x1 ->
  Gen x2 ->
  Gen x' ->
  Gen x1' ->
  Gen x2' ->
  Gen x'' ->
  Gen x1'' ->
  Gen x2'' ->
  [(PropertyName, Property)]
duoid_laws
  label
  law
  trans
  transL
  transR
  trans'
  transL'
  transR'
  trans''
  transD
  transS
  display
  dispL
  dispR
  display'
  dispL'
  dispR'
  display''
  dispD
  dispS
  genX
  genX1
  genX2
  genX'
  genX1'
  genX2'
  genX''
  genX1''
  genX2'' =
    monoid_laws label (diamond law) trans transL transR display dispL dispR genX genX1 genX2
      <> monoid_laws label (star law) trans' transL' transR' display' dispL' dispR' genX' genX1' genX2'
      <> [ ( "interchange (" <> label <> ")",
             property (runHH (checkLaw trans'' (interchange' law)) =<< forAllWith display'' $ genX'')
           ),
           ( "diamond unit (" <> label <> ")",
             property (runHH (checkLaw transD (diamondUnit law)) =<< forAllWith dispD $ genX1'')
           ),
           ( "star unit (" <> label <> ")",
             property (runHH (checkLaw transS (starUnit law)) =<< forAllWith dispS $ genX2'')
           ),
           ( "unit (" <> label <> ")",
             property (runHH (checkLaw transD (unit' law)) =<< forAllWith dispD $ genX1'')
           )
         ]

lattice_laws ::
  forall c a x x0 y.
  (c ~ (->), Eq y, Show y) =>
  PropertyName ->
  LatticeLaws c a ->
  (Prod c a a `c` a -> x `HH` y) ->
  (Prod c (Prod c a a) a `c` a -> x0 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen y ->
  [(PropertyName, Property)]
lattice_laws label law trans transA transY display dispA genX genX0 genY =
  semilattice_laws
    ("meet " <> label)
    (Lattice.meetSemilattice law)
    (trans . promap @c @c (bimap Meet Meet) getMeet)
    (transA . promap @c @c (bimap @c @c @c (bimap Meet Meet) Meet) getMeet)
    (transY . promap @c @c Meet getMeet)
    display
    dispA
    genX
    genX0
    genY
    <> semilattice_laws
      ("join " <> label)
      (Lattice.joinSemilattice law)
      (trans . promap @c @c (bimap Join Join) getJoin)
      (transA . promap @c @c (bimap @c @c @c (bimap Join Join) Join) getJoin)
      (transY . promap @c @c Join getJoin)
      display
      dispA
      genX
      genX0
      genY

boundedLattice_laws ::
  forall c a x x0 x1 x2 xx y.
  (c ~ (->), Eq y, Show y) =>
  PropertyName ->
  BoundedLatticeLaws c a ->
  (Prod c a a `c` a -> x `HH` y) ->
  (Prod c (Prod c a a) a `c` a -> x0 `HH` y) ->
  (a `c` a -> y `HH` y) ->
  (Prod c (Unit c (Prod c)) a `c` a -> x1 `HH` y) ->
  (Prod c a (Unit c (Prod c)) `c` a -> x2 `HH` y) ->
  (Prod c a (Prod c a a) `c` a -> xx `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  (xx -> String) ->
  Gen x ->
  Gen x0 ->
  Gen x1 ->
  Gen x2 ->
  Gen xx ->
  Gen y ->
  [(PropertyName, Property)]
boundedLattice_laws label law trans transA transI transL transR transG display dispA dispL dispR dispG genX genX0 genX1 genX2 genXX genY =
  lattice_laws label (lattice law) trans transA transI display dispA genX genX0 genY
    <> boundedSemilattice_laws
      ("meet " <> label)
      (BoundedLattice.meetSemilattice law)
      (trans . promap @c @c (bimap Meet Meet) getMeet)
      (transA . promap @c @c (bimap @c @c @c (bimap Meet Meet) Meet) getMeet)
      (transL . promap @c @c (bimap @c id Meet) getMeet)
      (transR . promap @c @c (bimap @_ @c Meet id) getMeet)
      (transI . promap @c @c Meet getMeet)
      (transG . promap @c @c (bimap @c @c @c Meet (bimap Meet Meet)) getMeet)
      display
      dispA
      dispL
      dispR
      dispG
      genX
      genX0
      genX1
      genX2
      genXX
      genY
    <> boundedSemilattice_laws
      ("join " <> label)
      (BoundedLattice.joinSemilattice law)
      (trans . promap @c @c (bimap Join Join) getJoin)
      (transA . promap @c @c (bimap @c @c @c (bimap Join Join) Join) getJoin)
      (transL . promap @c @c (bimap @c id Join) getJoin)
      (transR . promap @c @c (bimap @_ @c Join id) getJoin)
      (transI . promap @c @c Join getJoin)
      (transG . promap @c @c (bimap @c @c @c Join (bimap Join Join)) getJoin)
      display
      dispA
      dispL
      dispR
      dispG
      genX
      genX0
      genX1
      genX2
      genXX
      genY

-- |
--  __TODO__: Missing a lot of intermediate types here.
rig_laws ::
  forall c t a x x0 x1 x2 y.
  (c ~ (->), BraidedMonoidalCategory c t, Eq y, Show y) =>
  PropertyName ->
  RigLaws c t a ->
  (t a a `c` a -> x `HH` y) ->
  (t (t a a) a `c` a -> x0 `HH` y) ->
  (t (Unit c t) a `c` a -> x1 `HH` y) ->
  (t a (Unit c t) `c` a -> x2 `HH` y) ->
  (x -> String) ->
  (x0 -> String) ->
  (x1 -> String) ->
  (x2 -> String) ->
  Gen x ->
  Gen x0 ->
  Gen x1 ->
  Gen x2 ->
  [(PropertyName, Property)]
rig_laws label law trans transA transL transR display dispA dispL dispR genX genX0 genX1 genX2 =
  commutativeMonoid_laws
    ("additive " <> label)
    (Rig.commutativeMonoid law)
    (trans . promap @c @c (bimap Add Add) sum)
    (transA . promap @c @c (bimap @c @c @c (bimap Add Add) Add) sum)
    (transL . promap @c @c (bimap @c id Add) sum)
    (transR . promap @c @c (bimap @_ @c Add id) sum)
    display
    dispA
    dispL
    dispR
    genX
    genX0
    genX1
    genX2
    <> monoid_laws
      ("multiplicative " <> label)
      (Rig.monoid law)
      (transA . promap @c @c (bimap @c @c @c (bimap Multiply Multiply) Multiply) product)
      (transL . promap @c @c (bimap @c id Multiply) product)
      (transR . promap @c @c (bimap @_ @c Multiply id) product)
      dispA
      dispL
      dispR
      genX0
      genX1
      genX2

-- | Converts a profunctor category to `HH` for testing.
lowerDT :: (a `c` b -> y) -> DinaturalTransformation (->) f c -> f a b `HH` y
lowerDT post morphism = HH (post . runDT morphism)

semigroupoid_laws ::
  forall ok (c :: ok -> ok -> Type) (a :: ok) (b :: ok) y.
  (Eq y, Show y) =>
  PropertyName ->
  SemigroupoidLaws c ->
  (a `c` b -> y) ->
  (Procompose (Procompose c c) c a b -> String) ->
  Gen (Procompose (Procompose c c) c a b) ->
  [(PropertyName, Property)]
semigroupoid_laws label law post =
  semigroup_laws label law (lowerDT post)

category_laws ::
  forall ok (c :: ok -> ok -> Type) (a :: ok) (b :: ok) y.
  (Eq y, Show y) =>
  PropertyName ->
  CategoryLaws c ->
  (a `c` b -> y) ->
  (Procompose (Procompose c c) c a b -> String) ->
  (Procompose (:~:) c a b -> String) ->
  (Procompose c (:~:) a b -> String) ->
  Gen (Procompose (Procompose c c) c a b) ->
  Gen (Procompose (:~:) c a b) ->
  Gen (Procompose c (:~:) a b) ->
  [(PropertyName, Property)]
category_laws label law post =
  monoid_laws label law (lowerDT post) (lowerDT post) (lowerDT post)

semigroupalCategory_laws ::
  forall ok (c :: ok -> ok -> Type) (t :: ok -> ok -> ok) (a :: ok) (b :: ok) m n o x3 x4 y.
  (Ob c m, Ob c n, Ob c o, Eq y, Show y, Eq x3, Show x3, Eq x4, Show x4) =>
  PropertyName ->
  SemigroupalCategoryLaws c t ->
  (t m (t n o) `c` t m (t n o) -> x3 `HH` x3) ->
  (t (t m n) o `c` t (t m n) o -> x4 `HH` x4) ->
  (a `c` b -> y) ->
  (Procompose (Procompose c c) c a b -> String) ->
  (Procompose (:~:) c a b -> String) ->
  (Procompose c (:~:) a b -> String) ->
  Gen (Procompose (Procompose c c) c a b) ->
  Gen (Procompose (:~:) c a b) ->
  Gen (Procompose c (:~:) a b) ->
  Gen x3 ->
  Gen x4 ->
  [(PropertyName, Property)]
semigroupalCategory_laws label law trans1 trans2 post display dispL dispR genX genX1 genX2 genX3 genX4 =
  category_laws (label <> " category") (Semigroupal.category law) post display dispL dispR genX genX1 genX2
    <> isomorphism_laws (label <> " associative tensor") (Semigroupal.associative law) trans1 trans2 genX3 genX4

isomorphism_laws ::
  (Eq x1, Show x1, Eq x2, Show x2) =>
  PropertyName ->
  IsomorphismLaws c a b ->
  (a `c` a -> x1 `HH` x1) ->
  (b `c` b -> x2 `HH` x2) ->
  Gen x1 ->
  Gen x2 ->
  [(PropertyName, Property)]
isomorphism_laws label law trans1 trans2 genX1 genX2 =
  [ ( "forward isomorphism (" <> label <> ")",
      property (runHH (checkLaw trans1 (forward law)) =<< forAll genX1)
    ),
    ( "backward isomorphism (" <> label <> ")",
      property (runHH (checkLaw trans2 (backward law)) =<< forAll genX2)
    )
  ]

-- Would love to have a function like
-- forall a b y. (Eq y, Show y) => a `c` b ->

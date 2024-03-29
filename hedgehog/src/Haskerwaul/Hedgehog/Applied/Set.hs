{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}

module Haskerwaul.Hedgehog.Applied.Set where

import safe Control.Applicative
import safe Data.Bool (Bool)
import safe Data.Constraint ((\\))
import safe Data.Either
import safe Data.Int (Int16, Int32, Int8)
import safe Data.Type.Equality ((:~:) (..))
import safe Haskerwaul hiding (pure, ($))
import safe Haskerwaul.Category.Semigroupal.Laws
import Haskerwaul.Hedgehog
import Haskerwaul.Hedgehog.Topos
import safe Haskerwaul.Lattice.Bounded.Laws
import safe Haskerwaul.Lattice.Laws
import safe Haskerwaul.Monoid.Commutative.Laws
import safe Haskerwaul.Monoid.Laws
import safe Haskerwaul.Rig.Laws
import Hedgehog
import qualified Hedgehog.Function as Fn
import qualified Hedgehog.Gen as Gen
import safe Text.Show (show)
import safe Prelude (Eq, Show, ($))

genTuple :: Gen a -> Gen b -> Gen (a, b)
genTuple = liftA2 (,)

genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither a b = Gen.choice [Left <$> a, Right <$> b]

semigroupalCategory_lawsFn :: forall t. (SemigroupalCategory (->) t, TOb Eq t, TOb Show t) => PropertyName -> (forall a b. Gen a -> Gen b -> Gen (t a b)) -> [(PropertyName, Property)]
semigroupalCategory_lawsFn name genT =
  semigroupalCategory_laws
    name
    semigroupalCategoryLaws
    HH
    HH
    ($ 150) -- __FIXME__: Don't test everything with the same input!
    (const "can't show") -- __FIXME__: Show these things.
    (const "can't show")
    (const "can't show")
    ( Procompose
        <$> ( Procompose
                <$> (Fn.apply <$> Fn.fn @Int8 (Gen.enumBounded :: Gen Bool))
                <*> (Fn.apply <$> Fn.fn @Int16 (Gen.enumBounded :: Gen Int8))
            )
        <*> (Fn.apply <$> Fn.fn @Int32 (Gen.enumBounded :: Gen Int16))
    )
    (Procompose Refl . Fn.apply <$> Fn.fn @Int32 (Gen.enumBounded :: Gen Bool))
    ( flip Procompose Refl . Fn.apply
        <$> Fn.fn @Int32 (Gen.enumBounded :: Gen Bool)
    )
    (genT (Gen.enumBounded :: Gen Int8) (genT (Gen.enumBounded :: Gen Bool) (pure ())))
    (genT (genT (Gen.enumBounded :: Gen Int8) (Gen.enumBounded :: Gen Bool)) (pure ()))
    \\ inT @Eq @t @(t Int8 Bool) @()
    \\ inT @Eq @t @Int8 @(t Bool ())
    \\ inT @Eq @t @Int8 @Bool
    \\ inT @Eq @t @Bool @()
    \\ inT @Show @t @(t Int8 Bool) @()
    \\ inT @Show @t @Int8 @(t Bool ())
    \\ inT @Show @t @Int8 @Bool
    \\ inT @Show @t @Bool @()

monoid_lawsFn :: forall t x. (MonoidalCategory (->) t, Monoid (->) t x, Eq x, Show x, Show (Unit (->) t), TOb Show t) => (forall a b. Gen a -> Gen b -> Gen (t a b)) -> Gen (Unit (->) t) -> PropertyName -> Gen x -> [(PropertyName, Property)]
monoid_lawsFn genT genU labl gen =
  monoid_laws labl monoidLaws HH HH HH show show show (genT (genT gen gen) gen) (genT genU gen) (genT gen genU)
    \\ inT @Show @t @(t x x) @x
    \\ inT @Show @t @x @x
    \\ inT @Show @t @(Unit (->) t) @x
    \\ inT @Show @t @x @(Unit (->) t)

monoid_lawsEither ::
  (Monoid (->) Either x, Eq x, Show x) =>
  PropertyName ->
  Gen x ->
  [(PropertyName, Property)]
monoid_lawsEither = monoid_lawsFn genEither Gen.discard

monoid_lawsTup ::
  (Monoid (->) (,) x, Eq x, Show x) =>
  PropertyName ->
  Gen x ->
  [(PropertyName, Property)]
monoid_lawsTup = monoid_lawsFn genTuple (pure ())

commutativeMonoid_lawsFn :: forall t x. (BraidedMonoidalCategory (->) t, CommutativeMonoid (->) t x, Eq x, Show x, Show (Unit (->) t), TOb Show t) => (forall a b. Gen a -> Gen b -> Gen (t a b)) -> Gen (Unit (->) t) -> PropertyName -> Gen x -> [(PropertyName, Property)]
commutativeMonoid_lawsFn genT genU labl gen =
  commutativeMonoid_laws labl commutativeMonoidLaws HH HH HH HH show show show show (genT gen gen) (genT (genT gen gen) gen) (genT genU gen) (genT gen genU)
    \\ inT @Show @t @(t x x) @x
    \\ inT @Show @t @x @x
    \\ inT @Show @t @(Unit (->) t) @x
    \\ inT @Show @t @x @(Unit (->) t)

commutativeMonoid_lawsEither ::
  (CommutativeMonoid (->) Either x, Eq x, Show x) =>
  PropertyName ->
  Gen x ->
  [(PropertyName, Property)]
commutativeMonoid_lawsEither = commutativeMonoid_lawsFn genEither Gen.discard

commutativeMonoid_lawsTup ::
  (CommutativeMonoid (->) (,) x, Eq x, Show x) =>
  PropertyName ->
  Gen x ->
  [(PropertyName, Property)]
commutativeMonoid_lawsTup = commutativeMonoid_lawsFn genTuple (pure ())

lattice_lawsTup :: forall x. (Lattice (->) (,) x, Eq x, Show x) => PropertyName -> Gen x -> [(PropertyName, Property)]
lattice_lawsTup labl gen =
  lattice_laws labl latticeLaws HH HH HH show show (genTuple gen gen) (genTuple (genTuple gen gen) gen) gen

boundedLattice_lawsTup :: forall x. (BoundedLattice (->) (,) x, Eq x, Show x) => PropertyName -> Gen x -> [(PropertyName, Property)]
boundedLattice_lawsTup labl gen =
  boundedLattice_laws labl boundedLatticeLaws HH HH HH HH HH HH show show show show show (genTuple gen gen) (genTuple (genTuple gen gen) gen) (genTuple (pure ()) gen) (genTuple gen (pure ())) (genTuple gen (genTuple gen gen)) gen

rig_lawsFn :: forall t x. (BraidedMonoidalCategory (->) t, Rig (->) t x, Eq x, Show x, Show (Unit (->) t), TOb Show t) => (forall a b. Gen a -> Gen b -> Gen (t a b)) -> Gen (Unit (->) t) -> PropertyName -> Gen x -> [(PropertyName, Property)]
rig_lawsFn genT genU labl gen =
  rig_laws labl rigLaws HH HH HH HH show show show show (genT gen gen) (genT (genT gen gen) gen) (genT genU gen) (genT gen genU)
    \\ inT @Show @t @(t x x) @x
    \\ inT @Show @t @x @x
    \\ inT @Show @t @(Unit (->) t) @x
    \\ inT @Show @t @x @(Unit (->) t)

rig_lawsTup ::
  (Rig (->) (,) x, Eq x, Show x) =>
  PropertyName ->
  Gen x ->
  [(PropertyName, Property)]
rig_lawsTup = rig_lawsFn genTuple (pure ())

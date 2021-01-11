{-# language TypeApplications #-}

module Haskerwaul.Hedgehog.Applied.Set where

import Control.Applicative
import Data.Bool (Bool)
import Data.Constraint ((\\))
import Data.Constraint.Deferrable ((:~:)(..))
import Data.Either
import Data.Functor (fmap)
import Data.Int (Int16, Int32, Int8)
import Haskerwaul hiding (pure, ($))
import Haskerwaul.Category.Semigroupal.Laws
import Haskerwaul.Hedgehog
import Haskerwaul.Hedgehog.Topos
import Haskerwaul.Monoid.Commutative.Laws
import Haskerwaul.Monoid.Laws
import Haskerwaul.Rig.Laws
import Hedgehog
import qualified Hedgehog.Function as Fn
import qualified Hedgehog.Gen as Gen
import Prelude (Eq, Show, ($))
import Text.Show (show)

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
  (Procompose
    <$> (Procompose
          <$> (fmap Fn.apply $ Fn.fn @Int8 (Gen.enumBounded :: Gen Bool))
          <*> (fmap Fn.apply $ Fn.fn @Int16 (Gen.enumBounded :: Gen Int8)))
    <*> (fmap Fn.apply $ Fn.fn @Int32 (Gen.enumBounded :: Gen Int16)))
  (Procompose Refl
    <$> (fmap Fn.apply $ Fn.fn @Int32 (Gen.enumBounded :: Gen Bool)))
  (flip Procompose Refl
    <$> (fmap Fn.apply $ Fn.fn @Int32 (Gen.enumBounded :: Gen Bool)))
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

monoid_lawsEither
  :: (Monoid (->) Either x, Eq x, Show x)
  => PropertyName -> Gen x -> [(PropertyName, Property)]
monoid_lawsEither = monoid_lawsFn genEither Gen.discard

monoid_lawsTup
  :: (Monoid (->) (,) x, Eq x, Show x)
  => PropertyName -> Gen x -> [(PropertyName, Property)]
monoid_lawsTup = monoid_lawsFn genTuple (pure ())

commutativeMonoid_lawsFn :: forall t x. (BraidedMonoidalCategory (->) t, CommutativeMonoid (->) t x, Eq x, Show x, Show (Unit (->) t), TOb Show t) => (forall a b. Gen a -> Gen b -> Gen (t a b)) -> Gen (Unit (->) t) -> PropertyName -> Gen x -> [(PropertyName, Property)]
commutativeMonoid_lawsFn genT genU labl gen =
  commutativeMonoid_laws labl commutativeMonoidLaws HH HH HH HH show show show show (genT gen gen) (genT (genT gen gen) gen) (genT genU gen) (genT gen genU)
  \\ inT @Show @t @(t x x) @x
  \\ inT @Show @t @x @x
  \\ inT @Show @t @(Unit (->) t) @x
  \\ inT @Show @t @x @(Unit (->) t)

commutativeMonoid_lawsEither
  :: (CommutativeMonoid (->) Either x, Eq x, Show x)
  => PropertyName -> Gen x -> [(PropertyName, Property)]
commutativeMonoid_lawsEither = commutativeMonoid_lawsFn genEither Gen.discard

commutativeMonoid_lawsTup
  :: (CommutativeMonoid (->) (,) x, Eq x, Show x)
  => PropertyName -> Gen x -> [(PropertyName, Property)]
commutativeMonoid_lawsTup = commutativeMonoid_lawsFn genTuple (pure ())

rig_lawsFn :: forall t x. (BraidedMonoidalCategory (->) t, Rig (->) t x, Eq x, Show x, Show (Unit (->) t), TOb Show t) => (forall a b. Gen a -> Gen b -> Gen (t a b)) -> Gen (Unit (->) t) -> PropertyName -> Gen x -> [(PropertyName, Property)]
rig_lawsFn genT genU labl gen =
  rig_laws labl rigLaws HH HH HH HH show show show show (genT gen gen) (genT (genT gen gen) gen) (genT genU gen) (genT gen genU)
  \\ inT @Show @t @(t x x) @x
  \\ inT @Show @t @x @x
  \\ inT @Show @t @(Unit (->) t) @x
  \\ inT @Show @t @x @(Unit (->) t)

rig_lawsTup
  :: (Rig (->) (,) x, Eq x, Show x)
  => PropertyName -> Gen x -> [(PropertyName, Property)]
rig_lawsTup = rig_lawsFn genTuple (pure ())

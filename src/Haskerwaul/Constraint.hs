{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

-- | There is a
--  `Haskerwaul.Category.Monoidal.Cartesian.CartesianMonoidalCategory` of
--  `Constraint`-valued `Haskerwaul.Functor.Functor`s. Normally, the entire
--   hierarchy of instances would be defined here, but some components of this
--   category are fundamental to defining that hierarchy of type classes in the
--   first place, so the types are here, but the instances are with the type
--   classes.
module Haskerwaul.Constraint where

import           Data.Constraint ((:-)(..), (:=>)(..), Class(..), Dict(..))
import           Data.Kind (Constraint)

-- | A natural transformation in the category of constraints.
class ConstraintTransformation k (f :: ok -> Constraint) (g :: ok -> Constraint) a where
  constrainNT :: f a `k` g a

instance (f a :=> g a) => ConstraintTransformation (:-) f g a where
  constrainNT = ins

instance (f a :=> g a) :=> ConstraintTransformation (:-) f g a where
  ins = Sub Dict

-- | The `Haskerwaul.Category.Monoidal.Cartesian.Prod` for that category. The
--   name is for /C/onstraint-valued /F/unctor /Prod/uct.
class (con a, con' a) => CFProd con con' a

instance (con a, con' a) => CFProd con con' a

instance Class (con a, con' a) (CFProd con con' a) where
  cls = Sub Dict

instance (con a, con' a) :=> (CFProd con con' a) where
  ins = Sub Dict

-- | An empty constraint of any kind.
class    All a

instance All a

instance () :=> (All a) where
  ins = Sub Dict

-- | Compose an entailment with a constrained term to change the constraint
--   required.
(<+<) :: a => (b => c) -> (a :- b) -> c
x <+< Sub Dict = x
{-# INLINE (<+<) #-}

-- | Because `(,)` is handled oddly, we can't use it in
--  @`Haskerwaul.Category.Semigroupal.SemigroupalCategory` `(:-)` `(,)`@. This
--   is our workaround.
class Combine a b where

instance (a, b) => Combine a b

instance (a, b) :=> Combine a b where
  ins = Sub Dict

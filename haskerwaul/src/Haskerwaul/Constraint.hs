{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | There is a
--  `Haskerwaul.Category.Monoidal.Cartesian.CartesianMonoidalCategory` of
--  `Constraint`-valued `Haskerwaul.Functor.Functor`s. Normally, the entire
--   hierarchy of instances would be defined here, but some components of this
--   category are fundamental to defining that hierarchy of type classes in the
--   first place, so the types are here, but the instances are with the type
--   classes.
module Haskerwaul.Constraint where

import Data.Constraint (Class (cls), Dict (Dict), (:-) (Sub), (:=>) (ins))
import Data.Functor.Compose (Compose (getCompose))

-- | A natural transformation in the category of constraints.
class ((f a) => g a) => ConstraintTransformation f g a

instance ((f a) => g a) => ConstraintTransformation f g a

-- | The `Haskerwaul.Category.Monoidal.Cartesian.Prod` for that category. The
--   name is for /C/onstraint-valued /F/unctor /Prod/uct.
class (con a, con' a) => CFProd con con' a

instance (con a, con' a) => CFProd con con' a

instance Class (con a, con' a) (CFProd con con' a) where
  cls = Sub Dict

instance (con a, con' a) :=> CFProd con con' a where
  ins = Sub Dict

-- | An empty constraint of any kind.
class All a

instance All a

-- | Like `Data.Constraint.Bottom, but for @k -> `Data.Kind.Constraint`@. It
--   should be impossible to create an instance for this.
class None a where
  nope :: f a

none :: None a :- f a
none = Sub (getCompose nope)

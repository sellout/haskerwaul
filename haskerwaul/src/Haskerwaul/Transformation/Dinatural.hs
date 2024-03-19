{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Haskerwaul.Transformation.Dinatural where

import Data.Type.Equality ((:~:))
import Haskerwaul.Category.MonoidalUnit
import Haskerwaul.Constraint
import Haskerwaul.Object

-- | Like `NaturalTransformation`, but over a bifunctor.
--
-- = resources
--
-- - [nLab](https://ncatlab.org/nlab/show/dinatural+transformation)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Dinatural_transformation)
newtype DinaturalTransformation d f g = DT {runDT :: forall a b. f a b `d` g a b}

type instance Ob (DinaturalTransformation _) = All

-- | Like `FTensor`, but lifted from the target category to a bifunctor.
--
--  __NB__: With this definition, `d ~ (->)`, but that doesn't imply `dt ~ (,)`.
newtype BTensor dt f g a b = BTensor {lowerBTensor :: dt (f a b) (g a b)}

instance
  (MonoidalCategory' d dt) =>
  MonoidalCategory' (DinaturalTransformation d) (BTensor dt)
  where
  type Unit (DinaturalTransformation d) (BTensor dt) = BConst (Unit d dt)

-- | Like `Const`, but a bifunctor.
newtype BConst a b c = BConst {getBConst :: a}

-- | Composition of profunctors, a tensor in profunctor categories.
--
--  __TODO__: Is this any more general than the version in profunctors? If not,
--            maybe we should depend on that library (although maybe that
--            introduces too many other conflicting definitions).
data Procompose c d a b = forall z. Procompose (z `c` b) (a `d` z)

-- | The correct unit here is the Hom functor, but I don't know how to define
--   that, and this approach seems to work well enough for now.
instance MonoidalCategory' (DinaturalTransformation (->)) Procompose where
  type Unit (DinaturalTransformation (->)) Procompose = (:~:)

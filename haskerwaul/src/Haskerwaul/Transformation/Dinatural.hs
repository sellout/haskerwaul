{-# LANGUAGE TypeApplications #-}

module Haskerwaul.Transformation.Dinatural where

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Constraint
import Haskerwaul.Object

-- | Like `NaturalTransformation`, but over a bifunctor.
--
-- = resources
--
-- - [nLab](https://ncatlab.org/nlab/show/dinatural+transformation)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Dinatural_transformation)
newtype DinaturalTransformation d f g = DT {runDT :: forall a b. f a b `d` g a b}

type instance Ob (DinaturalTransformation d) = All

-- | Like `FTensor`, but lifted from the target category to a bifunctor.
--
--  __NB__: With this definition, `d ~ (->)`, but that doesn't imply `dt ~ (,)`.
data BTensor dt f g a b = BTensor {lowerBTensor :: dt (f a b) (g a b)}

instance
  (MonoidalCategory' d dt) =>
  MonoidalCategory' (DinaturalTransformation d) (BTensor dt)
  where
  type Unit (DinaturalTransformation d) (BTensor dt) = BConst (Unit d dt)

-- | Like `Const`, but a bifunctor.
newtype BConst a b c = BConst {getBConst :: a}

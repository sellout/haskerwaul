{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Symmetric
  ( module Haskerwaul.Category.Monoidal.Symmetric
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Balanced
  ) where

import           Data.Constraint ((:-))
import           Data.Either (Either(..))

import Haskerwaul.Category.Monoidal.Balanced
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/symmetric+monoidal+category)
class BalancedMonoidalCategory c t => SymmetricMonoidalCategory c t

-- | "switching things twice in the same direction has no effect"
--   -- [nLab](https://ncatlab.org/nlab/show/symmetric+monoidal+category#definition)
symmetric :: (SymmetricMonoidalCategory c t, Ob c a, Ob c b)
          => Isomorphism c (t a b) (t b a)
symmetric = Iso (to braid) (to braid)

-- | "switching things twice in the same direction has no effect"
--   -- [nLab](https://ncatlab.org/nlab/show/symmetric+monoidal+category#definition)
cosymmetric :: (SymmetricMonoidalCategory c t, Ob c a, Ob c b)
          => Isomorphism c (t b a) (t a b)
cosymmetric = Iso (from braid) (from braid)

instance SymmetricMonoidalCategory (->) (,)

instance SymmetricMonoidalCategory (->) Either

instance (d ~ (->), dt ~ (,), SymmetricMonoidalCategory d dt) =>
         SymmetricMonoidalCategory (NaturalTransformation c d) (FTensor dt)

instance SymmetricMonoidalCategory (:-) Combine

instance SymmetricMonoidalCategory (NaturalTransformation c (:-)) CFProd

instance (d ~ (->), SymmetricMonoidalCategory d dt) =>
         SymmetricMonoidalCategory (DinaturalTransformation d) (BTensor dt)

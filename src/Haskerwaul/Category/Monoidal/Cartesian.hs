{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cartesian
  ( module Haskerwaul.Category.Monoidal.Cartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Symmetric
  , module Haskerwaul.Object.Terminal
  ) where

import           Data.Constraint
  ((:-)(..), Dict(..), cls, trans, weaken1, weaken2)
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/cartesian+monoidal+category)
class ( SymmetricMonoidalCategory c (Prod c)
      , HasTerminalObject c
      , Unit c (Prod c) ~ TerminalObject c) =>
      CartesianMonoidalCategory c where
  -- | [nLab](https://ncatlab.org/nlab/show/cartesian+product)
  type Prod c :: ok -> ok -> ok
  -- | [nLab](https://ncatlab.org/nlab/show/projection)
  exl :: (Ob c a, Ob c b) => Prod c a b `c` a
  -- | [nLab](https://ncatlab.org/nlab/show/projection)
  exr :: (Ob c a, Ob c b) => Prod c a b `c` b
  -- | [nLab](https://ncatlab.org/nlab/show/diagonal+morphism)
  diagonal :: Ob c a => a `c` Prod c a a

instance CartesianMonoidalCategory (->) where
  type Prod (->) = (,)
  exl = Base.fst
  exr = Base.snd
  diagonal x = (x, x)

instance (d ~ (->), CartesianMonoidalCategory d) =>
         CartesianMonoidalCategory (NaturalTransformation c d) where
  type Prod (NaturalTransformation c d) = FTensor (Prod d)
  exl = NT (exl . lowerFTensor)
  exr = NT (exr . lowerFTensor)
  diagonal = NT (FTensor . diagonal)

instance CartesianMonoidalCategory (:-) where
  type Prod (:-) = Combine
  exl = trans weaken1 cls
  exr = trans weaken2 cls
  diagonal = Sub Dict

instance CartesianMonoidalCategory (NaturalTransformation c (:-)) where
  type Prod (NaturalTransformation c (:-)) = CFProd
  exl = NT (trans weaken1 cls)
  exr = NT (trans weaken2 cls)
  diagonal = NT (Sub Dict)

instance (d ~ (->), CartesianMonoidalCategory d) =>
         CartesianMonoidalCategory (DinaturalTransformation d) where
  type Prod (DinaturalTransformation d) = BTensor (Prod d)
  exl = DT (exl . lowerBTensor)
  exr = DT (exr . lowerBTensor)
  diagonal = DT (BTensor . diagonal)

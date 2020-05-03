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
import           Data.Either (Either(..))
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Subcategory.Full
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

instance (c ~ (->), CartesianMonoidalCategory c) =>
         CartesianMonoidalCategory (NaturalTransformation c) where
  type Prod (NaturalTransformation c) = FTensor (Prod c)
  exl = NT (exl . lowerFTensor)
  exr = NT (exr . lowerFTensor)
  diagonal = NT (FTensor . diagonal)

instance (CartesianMonoidalCategory c, TOb ob (Prod c), ob (TerminalObject c)) =>
         CartesianMonoidalCategory (FullSubcategory ob c) where
  type Prod (FullSubcategory ob c) = Prod c
  exl = FS exl
  exr = FS exr
  diagonal = FS diagonal

instance CartesianMonoidalCategory (:-) where
  type Prod (:-) = Combine
  exl = trans weaken1 cls
  exr = trans weaken2 cls
  diagonal = Sub Dict

instance CartesianMonoidalCategory (NaturalTransformation (:-)) where
  type Prod (NaturalTransformation (:-)) = CFProd
  exl = NT (trans weaken1 cls)
  exr = NT (trans weaken2 cls)
  diagonal = NT (Sub Dict)

-- * `CocartesianCategory` instances (in this module to avoid orphans)

instance CartesianMonoidalCategory (Opposite (->)) where
  type Prod (Opposite (->)) = Either
  exl = Opposite Left
  exr = Opposite Right
  diagonal =
    Opposite (\case
                 Left x -> x
                 Right x -> x)

-- instance CartesianMonoidalCategory (FullSubcategory (Monoid (->) (,)) (Opposite (->))) where
--   type Prod (FullSubcategory (Monoid (->) (,)) (Opposite (->))) = (,)
--   exl = FS (Opposite (, id))
--   exr = FS (Opposite (id, ))

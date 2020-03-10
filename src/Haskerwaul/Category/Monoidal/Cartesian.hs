{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cartesian
  ( module Haskerwaul.Category.Monoidal.Cartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Constraint
  ((:-)(..), Dict(..), cls, trans, weaken1, weaken2)
import           Data.Either (Either(..))
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Opposite
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/cartesian+monoidal+category)
class ( MonoidalCategory c (Prod c)
      , HasTerminalObject c
      , Unit c (Prod c) ~ TerminalObject c) =>
      CartesianMonoidalCategory c where
  type Prod c :: ok -> ok -> ok
  exl :: (Ob c a, Ob c b) => Prod c a b `c` a
  exr :: (Ob c a, Ob c b) => Prod c a b `c` b
  duplicate :: Ob c a => a `c` Prod c a a

instance CartesianMonoidalCategory (->) where
  type Prod (->) = (,)
  exl = Base.fst
  exr = Base.snd
  duplicate x = (x, x)

instance (CartesianMonoidalCategory c, TOb ob (Prod c), ob (TerminalObject c)) =>
         CartesianMonoidalCategory (FullSubcategory ob c) where
  type Prod (FullSubcategory ob c) = Prod c
  exl = FS exl
  exr = FS exr
  duplicate = FS duplicate

-- instance CartesianMonoidalCategory (:-) where
--   type Prod (:-) = Combine
--   exl = trans weaken1 ins
--   exr = trans weaken2 ins
--   duplicate = Sub Dict

instance CartesianMonoidalCategory (NaturalTransformation (:-)) where
  type Prod (NaturalTransformation (:-)) = CFProd
  exl = NT (trans weaken1 cls)
  exr = NT (trans weaken2 cls)
  duplicate = NT (Sub Dict)

-- * `CocartesianCategory` instances (in this module to avoid orphans)

instance CartesianMonoidalCategory (Opposite (->)) where
  type Prod (Opposite (->)) = Either
  exl = Opposite Left
  exr = Opposite Right
  duplicate =
    Opposite (\case
                 Left x -> x
                 Right x -> x)

-- instance CartesianMonoidalCategory (FullSubcategory (Monoid (->) (,)) (Opposite (->))) where
--   type Prod (FullSubcategory (Monoid (->) (,)) (Opposite (->))) = (,)
--   exl = FS (Opposite (, id))
--   exr = FS (Opposite (id, ))

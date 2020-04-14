{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

-- | This module exists to break the cycle with "Haskerwaul.Magma.Unital".
module Haskerwaul.Category.Monoidal' where

import           Data.Constraint ((:-))
import           Data.Either (Either(..))
import           Data.Kind (Type)
import           Data.Void (Void)

import Haskerwaul.Constraint
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+category)
class Ob c (Unit c t) =>
      MonoidalCategory' (c :: ok -> ok -> Type) (t :: ok -> ok -> ok) where
  -- | [nLab](https://ncatlab.org/nlab/show/unit+object)
  type Unit c t :: ok

instance MonoidalCategory' (->) (,) where
  type Unit (->) (,) = ()

instance MonoidalCategory' (->) Either where
  type Unit (->) Either = Void

instance MonoidalCategory' (:-) Combine where
  type Unit (:-) Combine = ()

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | This module exists to break the cycle with "Haskerwaul.Magma.Unital".
module Haskerwaul.Category.MonoidalUnit where

import Data.Constraint ((:-), type (&))
import Data.Either (Either (..))
import Data.Kind (Constraint, Type)
import Data.Void (Void)
import Haskerwaul.Object

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/monoidal+category)
class
  (Ob c (Unit c t)) =>
  MonoidalCategory' (c :: ok -> ok -> Type) (t :: ok -> ok -> ok)
  where
  -- |
  --
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/unit+object)
  type Unit c t :: ok

instance MonoidalCategory' (->) (,) where
  type Unit (->) (,) = ()

instance MonoidalCategory' (->) Either where
  type Unit (->) Either = Void

instance MonoidalCategory' (:-) (&) where
  type Unit (:-) (&) = (() :: Constraint)

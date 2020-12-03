{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Cartesian
  ( module Haskerwaul.Category.Closed.Cartesian
  -- * extended modules
  , module Haskerwaul.Category.Closed.Symmetric
  ) where

import qualified Control.Monad as Base
import qualified Data.Function as Base

import Haskerwaul.Category.Closed.Symmetric
import Haskerwaul.Object

-- | This is /not/ a "CCC". For that, see
--  `Haskerwaul.Category.Monoidal.Closed.Cartesian.CartesianClosedMonoidalCategory`.
--   The name clash forced a rename, and that was the easiest one to make.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/closed+category#symmetric_and_cartesian_versions)
class ClosedCategory c => CartesianClosedCategory c where
  const :: (Ob c a, Ob c b) => b `c` InternalHom c a b
  flattenHom :: (Ob c a, Ob c b) => InternalHom c a (InternalHom c a b) `c` InternalHom c a b

instance CartesianClosedCategory (->) where
  const = Base.const
  flattenHom = Base.join

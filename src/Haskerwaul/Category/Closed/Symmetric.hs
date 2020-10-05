{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Symmetric
  ( module Haskerwaul.Category.Closed.Symmetric
  -- * extended modules
  , module Haskerwaul.Category.Closed
  ) where

import qualified Data.Function as Base

import Haskerwaul.Category.Closed
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/closed+category#symmetric_and_cartesian_versions)
class ClosedCategory c => SymmetricClosedCategory c where
  -- | This is an involution.
  flip :: (Ob c x, Ob c y, Ob c z)
       => InternalHom c x (InternalHom c y z) `c` InternalHom c y (InternalHom c x z)

flipIso
  :: (SymmetricClosedCategory c, Ob c x, Ob c y, Ob c z)
  => Isomorphism c (InternalHom c x (InternalHom c y z)) (InternalHom c y (InternalHom c x z))
flipIso = Iso flip flip

instance SymmetricClosedCategory (->) where
  flip = Base.flip

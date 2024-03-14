{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Module.Right
  ( module Haskerwaul.Module.Right
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Ring
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Ring

class (Ring c t r, AbelianGroup c t m) => RightModule c t r m where
  rightScale :: t m r `c` m

-- -- | Every `CommutativeRig` forms a `Haskerwaul.Module.Module` with itself.
-- instance (c ~ (->), SemigroupalCategory c t, CommutativeRig c t r) =>
--          RightModule c t r r where
--   rightScale = multiply

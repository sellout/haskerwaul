{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Module.Left
  ( module Haskerwaul.Module.Left
  -- * extended modules
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Ring
  ) where

import Haskerwaul.Group.Abelian
import Haskerwaul.Ring

class (Ring c t r, AbelianGroup c t m) => LeftModule c t r m where
  leftScale :: t r m `c` m

-- -- | Every `CommutativeRig` forms a `Haskerwaul.Module.Module` with itself.
-- instance (c ~ (->), CommutativeRig c t r) => LeftModule c t r r where
--   leftScale = multiply

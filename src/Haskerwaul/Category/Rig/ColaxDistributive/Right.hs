{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig.ColaxDistributive.Right
  ( module Haskerwaul.Category.Rig.ColaxDistributive.Right
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Tuple as Base

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/colax-distributive+rig+category)
class (MonoidalCategory c p, MonoidalCategory c s) =>
      RightColaxDistributiveRigCategory c p s where
  rightDistribute :: (Ob c x, Ob c y, Ob c z)
                  => p (s x y) z `c` s (p x z) (p y z)
  rightAnnihilate :: Ob c a => Proxy s -> p (Unit c s) a `c` Unit c s

instance RightColaxDistributiveRigCategory (->) (,) Either where
  rightDistribute (e, c) = case e of
    Left a -> Left (a, c)
    Right b -> Right (b, c)
  rightAnnihilate Proxy = Base.fst

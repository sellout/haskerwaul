{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig.ColaxDistributive.Left
  ( module Haskerwaul.Category.Rig.ColaxDistributive.Left
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
      LeftColaxDistributiveRigCategory c p s where
  leftDistribute :: (Ob c x, Ob c y, Ob c z)
                 => p x (s y z) `c` s (p x y) (p x z)
  leftAnnihilate :: Ob c a => Proxy s -> p a (Unit c s) `c` Unit c s

instance LeftColaxDistributiveRigCategory (->) (,) Either where
  leftDistribute (a, e) = case e of
    Left b -> Left (a, b)
    Right c -> Right (a, c)
  leftAnnihilate Proxy = Base.snd

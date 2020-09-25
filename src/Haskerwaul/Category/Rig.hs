{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig
  ( module Haskerwaul.Category.Rig
  -- * extended modules
  , module Haskerwaul.Category.Rig.ColaxDistributive
  ) where

import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Void as Base

import Haskerwaul.Category.Rig.ColaxDistributive
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/rig+category)
class ColaxDistributiveRigCategory c p s => RigCategory c p s where
  leftFactor :: (Ob c x, Ob c y, Ob c z) => s (p x y) (p x z) `c` p x (s y z)
  rightFactor :: (Ob c x, Ob c y, Ob c z) => s (p x z) (p y z) `c` p (s x y) z
  leftBeget :: Ob c a => Proxy s -> Unit c s `c` p a (Unit c s)
  rightBeget :: Ob c a => Proxy s -> Unit c s `c` p (Unit c s) a

leftDistributive :: (RigCategory c p s, Ob c x, Ob c y, Ob c z)
                 => Isomorphism c (p x (s y z)) (s (p x y) (p x z))
leftDistributive = Iso leftDistribute leftFactor

rightDistributive :: (RigCategory c p s, Ob c x, Ob c y, Ob c z)
                  => Isomorphism c (p (s x y) z) (s (p x z) (p y z))
rightDistributive = Iso rightDistribute rightFactor

leftAnnihilation :: (RigCategory c p s, Ob c a)
                 => Proxy s -> Isomorphism c (p a (Unit c s)) (Unit c s)
leftAnnihilation p = Iso (leftAnnihilate p) (leftBeget p)

rightAnnihilation :: (RigCategory c p s, Ob c a)
                 => Proxy s -> Isomorphism c (p (Unit c s) a) (Unit c s)
rightAnnihilation p = Iso (rightAnnihilate p) (rightBeget p)

instance RigCategory (->) (,) Either where
  leftFactor = \case
    Left (a, b) -> (a, Left b)
    Right (a, c) -> (a, Right c)
  rightFactor = \case
    Left (a, c) -> (Left a, c)
    Right (b, c) -> (Right b, c)
  leftBeget Proxy = Base.absurd
  rightBeget Proxy = Base.absurd

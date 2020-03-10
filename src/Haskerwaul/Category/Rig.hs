{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig
  ( module Haskerwaul.Category.Rig
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Tuple as Base
import qualified Data.Void as Base

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/rig+category)
class (MonoidalCategory c p, MonoidalCategory c s) => RigCategory c p s where
  leftDistribute :: (Ob c x, Ob c y, Ob c z)
                 => Isomorphism c (p x (s y z)) (s (p x y) (p x z))
  rightDistribute :: (Ob c x, Ob c y, Ob c z)
                  => Isomorphism c (p (s x y) z) (s (p x z) (p y z))
  leftAnnihilate :: Ob c a
                 => Proxy s -> Isomorphism c (p a (Unit c s)) (Unit c s)
  rightAnnihilate :: Ob c a
                  => Proxy s -> Isomorphism c (p (Unit c s) a) (Unit c s)

instance RigCategory (->) (,) Either where
  leftDistribute =
    Iso
    (\(a, e) -> case e of
                  Left b -> Left (a, b)
                  Right c -> Right (a, c))
    (\case
        Left (a, b) -> (a, Left b)
        Right (a, c) -> (a, Right c))
  rightDistribute =
    Iso
    (\(e, c) -> case e of
                  Left a -> Left (a, c)
                  Right b -> Right (b, c))
    (\case
        Left (a, c) -> (Left a, c)
        Right (b, c) -> (Right b, c))
  leftAnnihilate Proxy = Iso Base.snd Base.absurd
  rightAnnihilate Proxy = Iso Base.fst Base.absurd

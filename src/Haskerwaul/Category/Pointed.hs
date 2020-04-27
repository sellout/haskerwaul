{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Pointed
  ( module Haskerwaul.Category.Pointed
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import Haskerwaul.Category
import Haskerwaul.Category.Opposite
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Initial

-- | [nLab](https://ncatlab.org/nlab/show/pointed+category)
class (Category c, HasTerminalObject c, HasTerminalObject (Opposite c)) =>
      PointedCategory c where
  -- | Must be the inverse of `(!)` specialized to @`InitialObject` c@.
  (!-) :: TerminalObject c `c` InitialObject c

zeroIso
  :: PointedCategory c => Isomorphism c (InitialObject c) (TerminalObject c)
zeroIso = Iso (!) (!-)

zeroMorphism :: (PointedCategory c, Ob c a, Ob c b) => a `c` b
zeroMorphism = opposite (!) . (!-) . (!)

instance (Category c, HasTerminalObject (Isomorphism c)) =>
         PointedCategory (Isomorphism c) where
  (!-) = Iso (opposite (to (!))) (opposite (from (!)))

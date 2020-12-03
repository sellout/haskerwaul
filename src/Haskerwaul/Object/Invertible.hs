{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Object.Invertible
  ( module Haskerwaul.Object.Invertible
  -- * extended modules
  , module Haskerwaul.Object.Dualizable
  ) where

import           Data.Kind (Type)

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Object.Dualizable

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/invertible+object)
--
--  __NB__: Instances for this are automatically coalesced.
class (Dualizable c t a, LeftDual c t a ~ RightDual c t a, Ob c (Dual c t a)) => Invertible (c :: ok -> ok -> Type) t a where
  type Dual c t a :: ok

instance (Dualizable c t a, LeftDual c t a ~ RightDual c t a, Ob c (Dual c t a)) => Invertible (c :: ok -> ok -> Type) t a where
  type Dual c t a = LeftDual c t a

leftInverseIso :: Invertible c t a => Isomorphism c (t (Dual c t a) a) (Unit c t)
leftInverseIso = Iso rightCounit leftCounit

rightInverseIso :: Invertible c t a => Isomorphism c (t a (Dual c t a)) (Unit c t)
rightInverseIso = Iso leftUnit rightUnit

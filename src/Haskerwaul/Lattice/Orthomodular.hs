{-# language UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Orthomodular
  ( module Haskerwaul.Lattice.Orthomodular
  -- * extended modules
  , module Haskerwaul.Lattice.Orthocomplemented
  ) where

import           Data.Bool (Bool(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Lattice.Orthocomplemented

-- | [nLab](https://ncatlab.org/nlab/show/complemented+lattice)
class OrthocomplementedLattice c t a => OrthomodularLattice c t a

-- | With a lawful instance, this should always return `True` when the provided
--   function is an equivalence relation.
orthomodular
  :: (OrthomodularLattice (->) (,) a) => ((a, a) -> Bool) -> a -> a -> Bool
orthomodular eq a c =
  if a <= c
  then eq (join (a, meet (complement (Proxy :: Proxy (,)) a, c)), c)
  else True
 where
  x <= y = eq (meet (x, y), x)

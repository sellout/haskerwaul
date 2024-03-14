{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Lattice.Orthomodular
  ( module Haskerwaul.Lattice.Orthomodular,

    -- * extended modules
    module Haskerwaul.Lattice.Orthocomplemented,
  )
where

import Data.Bool (Bool (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Algebra.Heyting
import Haskerwaul.Lattice.Orthocomplemented

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/complemented+lattice)
class (OrthocomplementedLattice c t a) => OrthomodularLattice c t a

-- | With a lawful instance, this should always return `True` when the provided
--   function is an equivalence relation.
orthomodular ::
  (OrthomodularLattice (->) (,) a) => ((a, a) -> Bool) -> a -> a -> Bool
orthomodular eq a c =
  implies
    ( a <= c,
      eq (join (a, meet (complement (Proxy :: Proxy (,)) a, c)), c)
    )
  where
    x <= y = eq (meet (x, y), x)

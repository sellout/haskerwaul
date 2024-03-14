module Haskerwaul.Relation.Inequality.Denial
  ( module Haskerwaul.Relation.Inequality.Denial
  -- * extended modules
  , module Haskerwaul.Relation.Equality
  ) where

import           Data.Proxy (Proxy (..))

import Haskerwaul.Lattice.Orthocomplemented
import Haskerwaul.Relation.Equality
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/denial+inequality)
denialInequality
  :: forall c a
   . ( ElementaryTopos c
     , OrthocomplementedLattice c (Prod c) (Class c)
     , EqualityRelation c a)
  => BinaryRelation c a a
denialInequality = complement (Proxy :: Proxy (Prod c)) . eq

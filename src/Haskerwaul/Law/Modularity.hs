{-# language TypeApplications #-}

module Haskerwaul.Law.Modularity where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equivalence
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/modular+lattice)
--   (using modular law)
modularity
  :: forall c a b
   . ( Ob c (Prod c (Prod c a b) a), Ob c (Prod c a b), Ob c (Prod c b a)
     , ElementaryTopos c, Ob c a, Ob c b, Preorder c a, EquivalenceRelation c b)
  => Prod c a b `c` b
  -> Prod c b a `c` b
  -> Law c (Prod c (Prod c a b) a) (Class c)
modularity join' meet' =
  Law
  (true . (!))
  (implies
   . bimap
     (le @c . first @c p exl)
     (equiv @c
      . bimap
        (meet' . first @c p join')
        (join' . second @c p meet' . from assoc)
      . diagonal)
   . diagonal)
  where
    p = Proxy :: Proxy c

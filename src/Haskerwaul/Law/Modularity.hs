{-# language TypeApplications #-}

module Haskerwaul.Law.Modularity where

import           Data.Constraint ((\\))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Algebra.Heyting
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object
import Haskerwaul.Relation.Equality
import Haskerwaul.Topos.Elementary

-- | [nLab](https://ncatlab.org/nlab/show/modular+lattice)
--   (using modular law)
modularity
  :: forall c a b
  -- see `ElementaryTopos` for why the `HeytingAlgebra` constraint isn't implied
   . ( HeytingAlgebra c (Prod c) (Class c)
     , ElementaryTopos c, Ob c a, Ob c b, Preorder c a, EquivalenceRelation c b)
  => Prod c a b `c` b
  -> Prod c b a `c` b
  -> Law c EqualityRelation (Prod c (Prod c a b) a) (Class c)
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
  \\ inT @(Ob c) @(Prod c) @(Prod c a b) @a
  \\ inT @(Ob c) @(Prod c) @a @b
  \\ inT @(Ob c) @(Prod c) @b @a
  where
    p = Proxy :: Proxy c

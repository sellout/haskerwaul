{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Commutative
  ( module Haskerwaul.Monoid.Commutative,

    -- * extended modules
    module Haskerwaul.Monoid,
    module Haskerwaul.Semigroup.Commutative,
  )
where

import Haskerwaul.Monoid
import Haskerwaul.Semigroup.Commutative

-- | [nLab](https://ncatlab.org/nlab/show/commutative+monoid)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeSemigroup c t a, Monoid c t a) => CommutativeMonoid c t a

instance (CommutativeSemigroup c t a, Monoid c t a) => CommutativeMonoid c t a

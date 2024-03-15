{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Group.Abelian
  ( module Haskerwaul.Group.Abelian,

    -- * extended modules
    module Haskerwaul.Group,
    module Haskerwaul.Monoid.Commutative,
  )
where

import Haskerwaul.Group
import Haskerwaul.Loop.Commutative
import Haskerwaul.Monoid.Commutative

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/abelian+group)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeLoop c t a, CommutativeMonoid c t a, Group c t a) => AbelianGroup c t a

instance (CommutativeLoop c t a, CommutativeMonoid c t a, Group c t a) => AbelianGroup c t a

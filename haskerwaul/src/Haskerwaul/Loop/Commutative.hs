{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Loop.Commutative
  ( module Haskerwaul.Loop.Commutative,

    -- * extended modules
    module Haskerwaul.Loop,
    module Haskerwaul.Quasigroup.Invertible.Commutative,
  )
where

import Haskerwaul.Loop
import Haskerwaul.Quasigroup.Invertible.Commutative

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/commutative+loop)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeInvertibleQuasigroup c t a, Loop c t a) => CommutativeLoop c t a

instance
  (CommutativeInvertibleQuasigroup c t a, Loop c t a) =>
  CommutativeLoop c t a

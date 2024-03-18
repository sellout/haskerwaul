{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Commutative
  ( module Haskerwaul.Quasigroup.Commutative,

    -- * extended modules
    module Haskerwaul.Magma.Commutative,
    module Haskerwaul.Quasigroup,
  )
where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Quasigroup

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/commutative+quasigroup)
--
--  __NB__: Instances for this are automatically coalesced.
class (CommutativeMagma c t a, Quasigroup c t a) => CommutativeQuasigroup c t a

instance (CommutativeMagma c t a, Quasigroup c t a) => CommutativeQuasigroup c t a

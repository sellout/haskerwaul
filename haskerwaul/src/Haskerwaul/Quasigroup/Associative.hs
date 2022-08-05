{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Associative
  ( module Haskerwaul.Quasigroup.Associative,

    -- * extended modules
    module Haskerwaul.Quasigroup,
    module Haskerwaul.Semigroup,
  )
where

import Haskerwaul.Quasigroup
import Haskerwaul.Semigroup

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/commutative+quasigroup)
--
--  __NB__: Instances for this are automatically coalesced.
class (Quasigroup c t a, Semigroup c t a) => AssociativeQuasigroup c t a

instance (Quasigroup c t a, Semigroup c t a) => AssociativeQuasigroup c t a

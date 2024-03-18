{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Magma.Invertible.Commutative
  ( module Haskerwaul.Magma.Invertible.Commutative,

    -- * extended modules
    module Haskerwaul.Magma.Commutative,
    module Haskerwaul.Magma.Invertible,
  )
where

import Haskerwaul.Magma.Commutative
import Haskerwaul.Magma.Invertible

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/commutative+invertible+magma)
--
--  __NB__: Instances for this are automatically coalesced.
class
  (CommutativeMagma c t a, InvertibleMagma c t a) =>
  CommutativeInvertibleMagma c t a

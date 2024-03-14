{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Invertible.Commutative
  ( module Haskerwaul.Quasigroup.Invertible.Commutative
  -- * extended modules
  , module Haskerwaul.Magma.Invertible.Commutative
  , module Haskerwaul.Quasigroup.Commutative
  , module Haskerwaul.Quasigroup.Invertible
  ) where

import Haskerwaul.Magma.Invertible.Commutative
import Haskerwaul.Quasigroup.Commutative
import Haskerwaul.Quasigroup.Invertible

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/commutative+invertible+quasigroup)
--
--  __NB__: Instances for this are automatically coalesced.
class ( CommutativeInvertibleMagma c t a
      , CommutativeQuasigroup c t a
      , InvertibleQuasigroup c t a) =>
      CommutativeInvertibleQuasigroup c t a

instance ( CommutativeInvertibleMagma c t a
         , CommutativeQuasigroup c t a
         , InvertibleQuasigroup c t a) =>
         CommutativeInvertibleQuasigroup c t a

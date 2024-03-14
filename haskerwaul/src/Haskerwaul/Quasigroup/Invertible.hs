{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Invertible
  ( module Haskerwaul.Quasigroup.Invertible,

    -- * extended modules
    module Haskerwaul.Magma.Invertible,
    module Haskerwaul.Quasigroup,
  )
where

import Haskerwaul.Magma.Invertible
import Haskerwaul.Quasigroup

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/invertible+quasigroup)
--
--  __NB__: Instances for this are automatically coalesced.
class (InvertibleMagma c t a, Quasigroup c t a) => InvertibleQuasigroup c t a

instance (InvertibleMagma c t a, Quasigroup c t a) => InvertibleQuasigroup c t a

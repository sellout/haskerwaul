{-# LANGUAGE Safe #-}

module Haskerwaul.Magmoid.Unital
  ( module Haskerwaul.Magmoid.Unital,

    -- * extended modules
    module Haskerwaul.Magma.Unital,
    module Haskerwaul.Magmoid,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import Haskerwaul.Magma.Unital
import Haskerwaul.Magmoid
import Haskerwaul.Transformation.Dinatural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/unital+magmoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type UnitalMagmoid = UnitalMagma (DinaturalTransformation (->)) Procompose

-- | Just a bit of sugar over `unit`, when it's used categorcally.
id :: (UnitalMagmoid c) => a `c` a
id = runDT (unit (Proxy :: Proxy Procompose)) Refl

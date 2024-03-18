{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Haskerwaul.Magmoid
  ( module Haskerwaul.Magmoid,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Magma

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/magmoid)
--
--  __TODO__: This should have a @`Haskerwaul.Profunctor.Profunctor` c c c@
--            constraint, but there are troublesome instances, so we skip the
--            constraint here and add it on the instances that make use of it.
type Magmoid = HorizontalCategorification Magma

-- -- | If /C/ and /C'/ are `Magmoid` instances, then so is their product.
-- instance
--   (Magmoid c, Magmoid c') =>
--   Magma (DinaturalTransformation (->)) Procompose (c :**: c')
--   where
--   op = DT (\(Procompose (NT (ProdC f)) (NT (ProdC g))) -> NT (f . g))

-- -- | If /C/ and /C'/ are `Semicategory` instances, then so is their product.
-- instance
--   (Semicategory c, Semicategory c') =>
--   Semigroup (DinaturalTransformation (->)) Procompose (c :**: c')

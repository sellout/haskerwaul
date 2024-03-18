{-# LANGUAGE Safe #-}

module Haskerwaul.Categorification.Horizontal where

import Haskerwaul.Transformation.Dinatural

-- | Also known as “oidification”, this lifts an algebraic structure to a
--   categorical structure.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/horizontal+categorification)
type HorizontalCategorification ob =
  ob (DinaturalTransformation (->)) Procompose

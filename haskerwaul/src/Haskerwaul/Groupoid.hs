{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Groupoid
  ( module Haskerwaul.Groupoid,

    -- * extended modules
    module Haskerwaul.Category,
    module Haskerwaul.Group,
  )
where

import Haskerwaul.Category
import Haskerwaul.Group
import Haskerwaul.Transformation.Dinatural

-- | [nLab](https://ncatlab.org/nlab/show/groupoid)
--
--  __NB__: Instances for this are automatically coalesced.
class (Group (DinaturalTransformation (->)) Procompose a, Category a) => Groupoid a

instance (Group (DinaturalTransformation (->)) Procompose a, Category a) => Groupoid a

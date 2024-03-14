-- __NB__: Pattern matching for `op`, etc. has broken exhaustivity checking.
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Haskerwaul.Category.Empty where

import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import Haskerwaul.Constraint
import Haskerwaul.Groupoid
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Prelude (error)

-- | [nLab](https://ncatlab.org/nlab/show/empty+category)
--
--  __TODO__: We could maybe also model this as something like
--           @`Haskerwaul.Subcategory.Full.FullSubcategory` `None` `(:~:)`@.
data EmptyCategory a b

fromEmpty :: EmptyCategory a b -> a `c` b
fromEmpty = \case {}

-- | There are no objects in the empty category.
type instance Ob EmptyCategory = None

instance Magma (DinaturalTransformation (->)) Procompose EmptyCategory where
  op = DT (\case {})

instance Semigroup (DinaturalTransformation (->)) Procompose EmptyCategory

-- | __FIXME__: It should be impossible to apply this `unit`, because there is
--              no object that passes the `None` constraint. However, I don't
--              think that's checked here, and we probably _can_ trigger this
--             `error` easily. This is a pretty good illustration that `(:~:)`
--              is the wrong @`Unit` (`DinaturalTransformation` (->)) `Procompose`@.
instance UnitalMagma (DinaturalTransformation (->)) Procompose EmptyCategory where
  unit Proxy =
    DT
      ( \Refl ->
          error
            "There is no object, thus no identity morphism, in the empty category."
      )

instance LeftQuasigroup (DinaturalTransformation (->)) Procompose EmptyCategory where
  leftQuotient = DT (\case {})

instance RightQuasigroup (DinaturalTransformation (->)) Procompose EmptyCategory where
  rightQuotient = DT (\case {})

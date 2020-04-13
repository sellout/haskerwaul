-- __NB__: Pattern matching for `op`, etc. has broken exhaustivity checking.
{-# options_ghc -fno-warn-incomplete-patterns #-}

module Haskerwaul.Category.Empty where

import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           Prelude (error)

import Haskerwaul.Constraint
import Haskerwaul.Groupoid
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/empty+category)
--
--  __TODO__: We could maybe also model this as something like
--           @`Haskerwaul.Subcategory.Full.FullSubcategory` `None` `(:~:)`@.
data EmptyCategory a b

fromEmpty :: EmptyCategory a b -> a `c` b
fromEmpty = \case

-- | There are no objects in the empty category.
type instance Ob EmptyCategory = None

instance Magma (NaturalTransformation2 (->)) CProd EmptyCategory where
  op = NT2 (\case)

instance Semigroup (NaturalTransformation2 (->)) CProd EmptyCategory

-- | __FIXME__: It should be impossible to apply this `unit`, because there is
--              no object that passes the `None` constraint. However, I don't
--              think that's checked here, and we probably _can_ trigger this
--             `error` easily. This is a pretty good illustration that `(:~:)`
--              is the wrong @`Unit` (`NaturalTransformation2` (->)) `CProd`@.
instance UnitalMagma (NaturalTransformation2 (->)) CProd EmptyCategory where
  unit Proxy =
    NT2
    (\Refl ->
       error
       "There is no object, thus no identity morphism, in the empty category.")

instance LeftQuasigroup (NaturalTransformation2 (->)) CProd EmptyCategory where
  leftQuotient = NT2 (\case)

instance RightQuasigroup (NaturalTransformation2 (->)) CProd EmptyCategory where
  rightQuotient = NT2 (\case)

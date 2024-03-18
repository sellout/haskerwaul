{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Concrete
  ( module Haskerwaul.Category.Concrete,

    -- * extended modules
    module Haskerwaul.Category,
  )
where

import Haskerwaul.Category
import Haskerwaul.Functor.Faithful

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/concrete+category) --
--   [remark 2.3](https://ncatlab.org/nlab/show/concrete+category#definition)
--   generalizes this from __Set__ to any category /X/, and we do that here, so
--   the less general notion would be represented as @`ConcreteCategory` (->)@.
class (Category c, FaithfulFunctor c x f) => ConcreteCategory x f c

-- -- assuming  `Hom_2` is `hom(-, 2)`,`a @`Functor` (`Op` (->)) (->)@
-- -- | the opposite category of a concrete category is always concrete
-- --  (https://ncatlab.org/nlab/show/concrete+category#properties)
-- instance ConcreteCategory x f c =>
--          ConcreteCategory x (Compose Hom_2 f) (Opposite c)

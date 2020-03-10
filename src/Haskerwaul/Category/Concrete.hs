{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Concrete
  ( module Haskerwaul.Category.Concrete
  -- * extended modules
  , module Haskerwaul.Category
  ) where

import Haskerwaul.Category
import Haskerwaul.Functor.Faithful

-- | [nLab](https://ncatlab.org/nlab/show/concrete+category)
class (Category c, FaithfulFunctor c x f) => ConcreteCategory x f c where

-- -- assuming  `Hom_2` is `hom(-, 2)`,`a @`Functor` (`Op` (->)) (->)@
-- -- | the opposite category of a concrete category is always concrete
-- --  (https://ncatlab.org/nlab/show/concrete+category#properties)
-- instance ConcreteCategory x f c =>
--          ConcreteCategory x (Compose Hom_2 f) (Opposite c)
